pub mod opcodes;
// pub mod stack; // does not work yet
pub mod status;
#[cfg(test)]
mod test;

use self::status::{Flag, ProcessorStatus};
use std::collections::HashMap;

pub const STACK_OFFSET: u16 = 0x0100;
pub const STACK_RESET: u8 = 0xFD;

pub struct CPU {
    pub register_a: u8, // accumulator - stores results of arithmetic and logic operations
    pub register_x: u8, // register x - temporarily stores values
    pub register_y: u8, // register y - temporarily stores values
    pub status: ProcessorStatus, // a register with 7 flags indicating cpu status after each instruction
    pub program_counter: u16,    // program counter holds two bytes, points to next instructions
    pub stack_pointer: u8,
    memory: [u8; 0xFFFF],
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    Accumulator,
    Relative,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: ProcessorStatus::new(),
            program_counter: 0,
            stack_pointer: STACK_RESET,
            memory: [0; 0xFFFF],
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::Absolute_X => {
                let pos = self.mem_read_u16(self.program_counter);
                let addr = pos.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let pos = self.mem_read_u16(self.program_counter);
                let addr = pos.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);
                
                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                
                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::Accumulator => panic!("Attempting to extract address from accumulator addressing mode. Handle this separately."),
            AddressingMode::Relative => panic!("Attempting to extract absolute address from relative offset. Handle this separately."),
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status.reset();

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        // Load program code into memory, starting at 0x8000 address
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }
    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    // LDA - Load Accumulator
    // Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // LDX - Load X Register
    // Loads a byte of memory into the X register setting the zero and negative flags as appropriate.
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    // LDY - Load Y Register
    // Loads a byte of memory into the Y register setting the zero and negative flags as appropriate.
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    // STA - Store Accumulator
    // Stores the contents of the accumulator into memory.
    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    // STX - Store X Register
    // Stores the contents of the X register into memory.
    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    // STX - Store Y Register
    // Stores the contents of the Y register into memory.
    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    // TAX - Transfer Accumulator to X
    // Copies the current contents of the accumulator into the X register and sets the zero and negative flags as appropriate
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    // TAY - Transfer Accumulator to Y
    // Copies the current contents of the accumulator into the Y register and sets the zero and negative flags as appropriate
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    // TSX - Transfer Stack Pointer to X
    // Copies the current contents of the stack register into the X register and sets the zero and negative flags as appropriate.
    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    // TXA - Transfer X to accumulator
    // Copies the current contents of the X register into the accumulator and sets the zero and negative flags as appropriate
    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // TXS - Transfer X to Stack Pointer
    // Copies the current contents of the X register into the stack register and sets the zero and negative flags as appropriate.
    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
        self.update_zero_and_negative_flags(self.stack_pointer);
    }

    // TYA - Transfer Y to accumulator
    // Copies the current contents of the Y register into the accumulator and sets the zero and negative flags as appropriate
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // INX - Increment X Register
    // Adds one to the X register setting the zero and negative flags as appropriate
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    // ADC - Add with Carry
    // This instruction adds the contents of a memory location to the accumulator together with the carry bit. If overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.add_to_register_a(value);
    }

    // AND - Logical AND
    // A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // ASL - Arithmetic Shift Left
    // This operation shifts all the bits of the accumulator or memory contents one bit left. Bit 0 is set to 0 and bit 7 is placed in the carry flag. The effect of this operation is to multiply the memory contents by 2 (ignoring 2's complement considerations), setting the carry if the result will not fit in 8 bits.
    fn asl(&mut self, mode: &AddressingMode) {
        let (orig, result) = match mode {
            AddressingMode::Accumulator => {
                let orig = self.register_a;
                self.register_a = self.register_a << 1;
                (orig, self.register_a)
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let orig = self.mem_read(addr);
                let data = orig << 1;
                self.mem_write(addr, data);
                (orig, data)
            }
        };

        let bit_7_set = 0b1000_0000 & orig != 0;
        self.status.update(Flag::C, bit_7_set);

        self.update_zero_and_negative_flags(result);
    }

    // BCC - Branch if Carry Clear
    // If the carry flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bcc(&mut self) {
        if !self.status.get(Flag::C) {
            self.branch();
        }
    }

    // BCS - Branch if Carry Set
    // If the carry flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn bcs(&mut self) {
        if self.status.get(Flag::C) {
            self.branch();
        }
    }

    // BEQ - Branch if Equal
    // If the zero flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn beq(&mut self) {
        if self.status.get(Flag::Z) {
            self.branch();
        }
    }

    // BMI - Branch if Minus
    // If the negative flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn bmi(&mut self) {
        if self.status.get(Flag::N) {
            self.branch();
        }
    }

    // BNE - Branch not Equal
    // If the zero flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bne(&mut self) {
        if !self.status.get(Flag::Z) {
            self.branch();
        }
    }

    // BPL - Branch if Positive
    // If the negative flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bpl(&mut self) {
        if !self.status.get(Flag::N) {
            self.branch();
        }
    }

    // BVC - Branch if Overflow Clear
    // If the overflow flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bvc(&mut self) {
        if !self.status.get(Flag::V) {
            self.branch();
        }
    }

    // BVS - Branch if Overflow Set
    // If the overflow flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn bvs(&mut self) {
        if self.status.get(Flag::V) {
            self.branch();
        }
    }

    // BIT - Bit Test
    // This instructions is used to test if one or more bits are set in a target memory location. The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result is not kept. Bits 7 and 6 of the value from memory are copied into the N and V flags.
    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a & value;

        self.status.update(Flag::Z, result == 0);
        self.status.match_bit(Flag::V, value);
        self.status.match_bit(Flag::N, value);
    }

    // CLC - Clear Carry Flag
    // Set the carry flag to zero.
    fn clc(&mut self) {
        self.status.unset(Flag::C);
    }

    // CLD - Clear Decimal Mode
    // Set the decimal mode flag to zero.
    fn cld(&mut self) {
        self.status.unset(Flag::D);
    }

    // CLI - Clear Interrupt Disable
    // Set the interrupt disable flag to zero.
    fn cli(&mut self) {
        self.status.unset(Flag::I);
    }

    // CLV - Clear Overflow Flag
    // Set the overflow flag to zero.
    fn clv(&mut self) {
        self.status.unset(Flag::V);
    }

    // CMP - Compare
    // This instruction compares the contents of the accumulator with another memory held value and sets the zero and carry flags as appropriate.
    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.status.update(Flag::C, self.register_a >= data);
        self.status.update(Flag::Z, self.register_a == data);
        self.status.match_bit(Flag::N, self.register_a - data);
    }

    // CPX - Compare X Register
    // This instruction compares the contents of the X register with another memory held value and sets the zero and carry flags as appropriate.
    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.status.update(Flag::C, self.register_x >= data);
        self.status.update(Flag::Z, self.register_x == data);
        self.status.match_bit(Flag::N, self.register_x - data);
    }

    // CPY - Compare Y Register
    // This instruction compares the contents of the Y register with another memory held value and sets the zero and carry flags as appropriate.
    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.status.update(Flag::C, self.register_y >= data);
        self.status.update(Flag::Z, self.register_y == data);
        self.status.match_bit(Flag::N, self.register_y - data);
    }

    // DEC - Decrement Memory
    // Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        
        let result = data.wrapping_sub(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    // DEX - Decrement X Register
    // Subtracts one from the X register setting the zero and negative flags as appropriate.
    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    // DEX - Decrement Y Register
    // Subtracts one from the Y register setting the zero and negative flags as appropriate.
    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    // EOR - Exclusive OR
    // An exclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.register_a = self.register_a ^ data;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // INC - Increment Memory
    // Adds one to the value held at a specified memory location setting the zero and negative flags as appropriate.
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let result = data.wrapping_add(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    // INY - Increment Y Register
    // Adds one to the Y register setting the zero and negative flags as appropriate.
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn add_to_register_a(&mut self, data: u8) {
        let sum = self.register_a as u16
            + data as u16
            + if self.status.get(Flag::C) { 1 } else { 0 } as u16;
        let carry = sum > 0xff;
        self.status.update(Flag::C, carry);

        let result = sum as u8;
        let overflow = (data ^ result) & (result ^ self.register_a) & 0x80 != 0;
        self.status.update(Flag::V, overflow);

        self.register_a = result;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn branch(&mut self) {
        let jump: i8 = self.mem_read(self.program_counter) as i8;
        let jump_addr = self.program_counter.wrapping_add(1).wrapping_add(jump as u16);
        self.program_counter = jump_addr;
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write((STACK_OFFSET as u16) + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read((STACK_OFFSET as u16) + self.stack_pointer as u16)
    }

    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xFF) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;

        hi << 8 | lo
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.update(Flag::Z, result == 0);
        self.status.match_bit(Flag::N, result);
    }

    pub fn run(&mut self) {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
        loop {
            // Read from the ROM the byte at the address in the PC
            let code = self.mem_read(self.program_counter);
            // Increment the PC to the next instruction
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes
                .get(&code)
                .expect(&format!("OpCode {:x} is not recognized", code));

            match opcode.mnemonic {
                "LDA" => self.lda(&opcode.mode),
                "LDX" => self.ldx(&opcode.mode),
                "LDY" => self.ldy(&opcode.mode),
                "STA" => self.sta(&opcode.mode),
                "STX" => self.stx(&opcode.mode),
                "STY" => self.sty(&opcode.mode),
                "ADC" => self.adc(&opcode.mode),
                "AND" => self.and(&opcode.mode),
                "ASL" => self.asl(&opcode.mode),
                "BIT" => self.bit(&opcode.mode),
                "CMP" => self.cmp(&opcode.mode),
                "CPX" => self.cpx(&opcode.mode),
                "CPY" => self.cpy(&opcode.mode),
                "DEC" => self.dec(&opcode.mode),
                "EOR" => self.eor(&opcode.mode),
                "INC" => self.inc(&opcode.mode),
                "TAX" => self.tax(),
                "TAY" => self.tay(),
                "TSX" => self.tsx(),
                "TXA" => self.txa(),
                "TXS" => self.txs(),
                "TYA" => self.tya(),
                "INX" => self.inx(),
                "INY" => self.iny(),
                "BCC" => self.bcc(),
                "BCS" => self.bcs(),
                "BEQ" => self.beq(),
                "BMI" => self.bmi(),
                "BNE" => self.bne(),
                "BPL" => self.bpl(),
                "BVC" => self.bvc(),
                "BVS" => self.bvs(),
                "CLC" => self.clc(),
                "CLD" => self.cld(),
                "CLI" => self.cli(),
                "CLV" => self.clv(),
                "DEX" => self.dex(),
                "DEY" => self.dey(),
                "BRK" => return,
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }
}
