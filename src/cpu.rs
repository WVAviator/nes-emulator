pub mod opcodes;
// pub mod stack; // does not work yet
pub mod status;
#[cfg(test)]
mod test;

use crate::{bus::Bus, rom::Rom};

use self::{status::{Flag, ProcessorStatus}, opcodes::OpCode};
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
    pub bus: Bus,
    pub debug: bool,
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

pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
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
    fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }
    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }
    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }
    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data)
    }
}

/// Initializatino functions
impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: ProcessorStatus::new(),
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus: Bus::new(),
            debug: false,
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status.reset();

        self.program_counter = self.mem_read_u16(0xFFFC) - if self.debug { 4 } else { 0 };
        if self.debug {
            println!("Reset CPU. Reading new program start location {:0<4X}", self.program_counter);
        }
    }

    pub fn load_rom(&mut self, rom: Rom) {
        self.bus.load_cartridge(rom);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.load_rom(Rom::generate_standalone_rom(program));
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

}

/// Opcodes
impl CPU {

    /// AAC - (Unofficial)
    /// AND byte with accumulator. If result is negative then carry is set.
    /// Status flags: N,Z,C
    fn aac(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        self.update_zero_and_negative_flags(self.register_a);
        self.status.update(Flag::C, self.register_a & 0b1000_0000 == 0b1000_0000);
    }

    /// AAX - (Unofficial)
    /// AND X register with accumulator and store result in memory.
    /// Status flags: N,Z
    fn aax(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let result = self.register_a & self.register_x;
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    /// ADC - Add with Carry
    /// This instruction adds the contents of a memory location to the accumulator together with the carry bit. If overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.add_to_register_a(value);
    }

    /// AND - Logical AND
    /// A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// ARR - (Unofficial)
    /// AND byte with accumulator, then rotate one bit right in accu-mulator and
    /// check bit 5 and 6:
    /// If both bits are 1: set C, clear V.
    /// If both bits are 0: clear C and V.
    /// If only bit 5 is 1: set V, clear C.
    /// If only bit 6 is 1: set C and V.
    /// Status flags: N,V,Z,C
    fn arr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        self.register_a = self.register_a.rotate_right(1);
        let bit_5_set = self.register_a & 0b0100_0000 == 0b0100_0000;
        let bit_6_set = self.register_a & 0b0010_0000 == 0b0010_0000;
        match (bit_5_set, bit_6_set) {
            (true, true) => {
                self.status.set(Flag::C);
                self.status.unset(Flag::V);
            }
            (true, false) => {
                self.status.unset(Flag::C);
                self.status.set(Flag::V);
            }
            (false, true) => self.status.set_many(vec![Flag::C, Flag::V]),
            (false, false) => self.status.unset_many(vec![Flag::C, Flag::V]),
        }
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// ASL - Arithmetic Shift Left
    /// This operation shifts all the bits of the accumulator or memory contents one bit left. Bit 0 is set to 0 and bit 7 is placed in the carry flag. The effect of this operation is to multiply the memory contents by 2 (ignoring 2's complement considerations), setting the carry if the result will not fit in 8 bits.
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

    /// ASR - (Unofficial)
    /// AND byte with accumulator, then shift right one bit in accumu-lator.
    /// Status flags: N,Z,C
    fn asr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        let lsb = self.register_a & 0b0000_0001 == 0b0000_0001;
        self.register_a = self.register_a >> 1;
        if self.status.get(Flag::C) {
            self.register_a = self.register_a | 0b1000_0000;
        }
        self.status.update(Flag::C, lsb);
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// ATX - (Unofficial)
    /// AND byte with accumulator, then transfer accumulator to X register.
    /// Status flags: N,Z
    fn atx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// AXA - (Unofficial)
    /// AND X register with accumulator then AND result with 7 and store in memory.
    /// Status flags: -
    fn axa(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let result = self.register_x & self.register_a & 7;
        self.mem_write(addr, result);
    }

    // AXS - (Unofficial)
    /// AND X register with accumulator and store result in X regis-ter, 
    /// then subtract byte from X register (without borrow).
    /// Status flags: N,Z,C
    fn axs(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let x_and_a = self.register_x & self.register_a;
        self.register_x = x_and_a.wrapping_sub(data);

        if data <= x_and_a {
            self.status.set(Flag::C);
        }
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// BCC - Branch if Carry Clear
    /// If the carry flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bcc(&mut self) {
        if !self.status.get(Flag::C) {
            self.branch();
        }
    }

    /// BCS - Branch if Carry Set
    /// If the carry flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn bcs(&mut self) {
        if self.status.get(Flag::C) {
            self.branch();
        }
    }

    /// BEQ - Branch if Equal
    /// If the zero flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn beq(&mut self) {
        if self.status.get(Flag::Z) {
            self.branch();
        }
    }

    /// BIT - Bit Test
    /// This instructions is used to test if one or more bits are set in a target memory location. The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result is not kept. Bits 7 and 6 of the value from memory are copied into the N and V flags.
    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a & value;

        self.status.update(Flag::Z, result == 0);
        self.status.match_bit(Flag::V, value);
        self.status.match_bit(Flag::N, value);
    }

    /// BMI - Branch if Minus
    /// If the negative flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn bmi(&mut self) {
        if self.status.get(Flag::N) {
            self.branch();
        }
    }

    /// BNE - Branch not Equal
    /// If the zero flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bne(&mut self) {
        if !self.status.get(Flag::Z) {
            self.branch();
        }
    }

    /// BPL - Branch if Positive
    /// If the negative flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bpl(&mut self) {
        if !self.status.get(Flag::N) {
            self.branch();
        }
    }

    /// BVC - Branch if Overflow Clear
    /// If the overflow flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
    fn bvc(&mut self) {
        if !self.status.get(Flag::V) {
            self.branch();
        }
    }

    /// BVS - Branch if Overflow Set
    /// If the overflow flag is set then add the relative displacement to the program counter to cause a branch to a new location.
    fn bvs(&mut self) {
        if self.status.get(Flag::V) {
            self.branch();
        }
    }

    /// CLC - Clear Carry Flag
    /// Set the carry flag to zero.
    fn clc(&mut self) {
        self.status.unset(Flag::C);
    }

    /// CLD - Clear Decimal Mode
    /// Set the decimal mode flag to zero.
    fn cld(&mut self) {
        self.status.unset(Flag::D);
    }

    /// CLI - Clear Interrupt Disable
    /// Set the interrupt disable flag to zero.
    fn cli(&mut self) {
        self.status.unset(Flag::I);
    }

    /// CLV - Clear Overflow Flag
    /// Set the overflow flag to zero.
    fn clv(&mut self) {
        self.status.unset(Flag::V);
    }

    /// CMP - Compare
    /// This instruction compares the contents of the accumulator with another memory held value and sets the zero and carry flags as appropriate.
    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.status.update(Flag::C, self.register_a >= data);
        self.status.update(Flag::Z, self.register_a == data);
        self.status.match_bit(Flag::N, self.register_a.wrapping_sub(data));
    }

    /// CPX - Compare X Register
    /// This instruction compares the contents of the X register with another memory held value and sets the zero and carry flags as appropriate.
    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.status.update(Flag::C, self.register_x >= data);
        self.status.update(Flag::Z, self.register_x == data);
        self.status.match_bit(Flag::N, self.register_x.wrapping_sub(data));
    }

    /// CPY - Compare Y Register
    /// This instruction compares the contents of the Y register with another memory held value and sets the zero and carry flags as appropriate.
    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.status.update(Flag::C, self.register_y >= data);
        self.status.update(Flag::Z, self.register_y == data);
        self.status.match_bit(Flag::N, self.register_y.wrapping_sub(data));
    }

    /// DCP - (Unofficial)
    /// Decrements the value in memory, then CMPs the result with A
    /// Status flags: C
    /// The z and c flags are set according to this chart:
    /// A  <  value      0  0
    /// A  =  value      1  1
    /// A  >  value      0  1
    /// N is set if the high bit is set after A - value
    fn dcp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let result = data.wrapping_sub(1);
        self.mem_write(addr, result);

        self.status.update(Flag::C, result >= self.register_a);
        self.status.update(Flag::Z, result == self.register_a);
        self.status.match_bit(Flag::N, self.register_a.wrapping_sub(result));
    }

    /// DEC - Decrement Memory
    /// Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        
        let result = data.wrapping_sub(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    /// DEX - Decrement X Register
    /// Subtracts one from the X register setting the zero and negative flags as appropriate.
    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// DEX - Decrement Y Register
    /// Subtracts one from the Y register setting the zero and negative flags as appropriate.
    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// DOP - (Unofficial)
    /// No operation (double NOP). The argument has no significance.
    /// Status flags: -
    fn dop(&self) {}

    /// ISC (Unofficial)
    /// Increase memory by one, then subtract memory from accu-mulator (with borrow).
    /// n - Was the high bit of the result set?
    /// v - Was there signed overflow?
    /// v is set only if one of the following is true:
    ///   If both A and value are negative and the result is positive
    ///   If both A and value are positive and the result is negative
    /// z - Was the result 0?
    /// c - Was there overflow?
    fn isc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr).wrapping_add(1);
        self.mem_write(addr, data);

        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    /// EOR - Exclusive OR
    /// An exclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.register_a = self.register_a ^ data;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// INC - Increment Memory
    /// Adds one to the value held at a specified memory location setting the zero and negative flags as appropriate.
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let result = data.wrapping_add(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    /// INX - Increment X Register
    /// Adds one to the X register setting the zero and negative flags as appropriate
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// INY - Increment Y Register
    /// Adds one to the Y register setting the zero and negative flags as appropriate.
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// JMP - Jump
    /// Sets the program counter to the address specified by the operand.
    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.mem_read_u16(self.program_counter);

        match mode {
            AddressingMode::Absolute => {
                self.program_counter = addr;
            }
            _ => {
                // An original 6502 has does not correctly fetch the target address if the indirect vector falls on a page boundary (e.g. $xxFF where xx is any value from $00 to $FF). In this case fetches the LSB from $xxFF as expected but takes the MSB from $xx00. This is fixed in some later chips like the 65SC02 so for compatibility always ensure the indirect vector is not at the end of the page.

                let indirect_ref = if addr & 0x00FF == 0x00FF {
                    let lo = self.mem_read(addr);
                    let hi = self.mem_read(addr & 0xFF00);
                    (hi as u16) << 8 | (lo as u16)
                } else {
                    self.mem_read_u16(addr)
                };

                self.program_counter = indirect_ref;
            }
        }
    }

    /// JSR - Jump to Subroutine
    /// The JSR instruction pushes the address (minus one) of the return point on to the stack and then sets the program counter to the target memory address.
    fn jsr(&mut self) {
        self.stack_push_u16(self.program_counter.wrapping_add(1));
        let addr = self.mem_read_u16(self.program_counter);
        self.program_counter = addr;
    }

    /// KIL - (Unofficial)
    /// Stop program counter (processor lock up).
    fn kil(&self) {
        panic!("KIL invoked - processor locked up.");
    }

    /// LAR (Unofficial)
    /// AND memory with stack pointer, transfer result to accu-mulator, X register and stack pointer.
    fn lar(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.stack_pointer = self.stack_pointer & value;
        self.register_a = self.stack_pointer;
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// LAX - (Unofficial)
    /// Load accumulator and X register with memory.
    fn lax(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// LDA - Load Accumulator
    /// Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// LDX - Load X Register
    /// Loads a byte of memory into the X register setting the zero and negative flags as appropriate.
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// LDY - Load Y Register
    /// Loads a byte of memory into the Y register setting the zero and negative flags as appropriate.
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// LSR - Logical Shift Right
    /// Each of the bits in A or M is shift one place to the right. The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
    fn lsr(&mut self, mode: &AddressingMode) {
        let (orig, result) = match mode {
            AddressingMode::Accumulator => {
                let orig = self.register_a;
                self.register_a = self.register_a >> 1;
                (orig, self.register_a)
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let orig = self.mem_read(addr);
                let data = orig >> 1;
                self.mem_write(addr, data);
                (orig, data)
            }
        };

        let bit_0_set = 0b0000_0001 & orig != 0;
        self.status.update(Flag::C, bit_0_set);

        self.update_zero_and_negative_flags(result);
    }

    /// NOP - No Operation
    /// The NOP instruction causes no changes to the processor other than the normal incrementing of the program counter to the next instruction.
    fn nop(&self) { }

    /// ORA - Logical Inclusive OR
    /// An inclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.register_a = self.register_a | data;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// PHA - Push Accumulator
    /// Pushes a copy of the accumulator on to the stack.
    fn pha(&mut self) {
        self.stack_push(self.register_a);
    }

    /// PHP - Push Processor Status
    /// Pushes a copy of the status flags on to the stack.
    fn php(&mut self) {
        self.stack_push(self.status.value() | Flag::B.as_u8() | Flag::S.as_u8());
    }

    /// PLA - Pull Accumulator
    /// Pulls an 8 bit value from the stack and into the accumulator. The zero and negative flags are set as appropriate.
    fn pla(&mut self) {
        self.register_a = self.stack_pop();
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// PLP - Pull Processor Status
    /// Pulls an 8 bit value from the stack and into the processor flags. The flags will take on new states as determined by the value pulled.
    fn plp(&mut self) {
        let flags = self.stack_pop();
        self.status.restore(flags);
        self.status.unset(Flag::B);
        self.status.set(Flag::S);
    }

    /// RLA - (Unofficial)
    /// Rotate one bit left in memory, then AND accumulator with memory.
    fn rla(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let bit_7_set = data & 0b1000_0000 == 0b1000_0000;

        let result = if self.status.get(Flag::C) { 
            (data << 1) + 1
         } else {
            data << 1
         };
         self.status.update(Flag::C, bit_7_set);

         self.mem_write(addr, result);
         self.register_a = self.register_a & result;
         self.update_zero_and_negative_flags(self.register_a);
    }

    /// RRA - (Unoffical)
    /// Rotate one bit right in memory, then add memory to accumulator (with carry).
    fn rra(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let bit_1_set = data & 0b0000_0001 == 0b0000_0001;
        let result = if self.status.get(Flag::C) {
            (data >> 1) | 0b1000_0000
        } else {
            data >> 1
        };
        self.status.update(Flag::C, bit_1_set);
        self.mem_write(addr, result);

        self.add_to_register_a(result);

    }

    /// ROL - Rotate Left
    /// Move each of the bits in either A or M one place to the left. Bit 0 is filled with the current value of the carry flag whilst the old bit 7 becomes the new carry flag value.
    fn rol(&mut self, mode: &AddressingMode) {
        let (orig, result) = match mode {
            AddressingMode::Accumulator => {
                let orig = self.register_a;
                self.register_a = (self.register_a << 1) | self.status.as_bit_if_set(Flag::C);
                (orig, self.register_a)
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let orig = self.mem_read(addr);
                let data = (orig << 1) | self.status.as_bit_if_set(Flag::C);

                self.mem_write(addr, data);
                (orig, data)
            }
        };

        let bit_7_set = 0b1000_0000 & orig != 0;
        self.status.update(Flag::C, bit_7_set);

        self.update_zero_and_negative_flags(result);
    }

    /// ROR - Rotate Right
    /// Move each of the bits in either A or M one place to the right. Bit 7 is filled with the current value of the carry flag whilst the old bit 0 becomes the new carry flag value.
    fn ror(&mut self, mode: &AddressingMode) {
        let (orig, result) = match mode {
            AddressingMode::Accumulator => {
                let orig = self.register_a;
                self.register_a = (self.register_a >> 1) | if self.status.get(Flag::C) { 0b1000_0000 } else { 0 };
                (orig, self.register_a)
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let orig = self.mem_read(addr);
                let data = (orig >> 1) | if self.status.get(Flag::C) { 0b1000_0000 } else { 0 };

                self.mem_write(addr, data);
                (orig, data)
            }
        };

        let bit_0_set = 0b0000_0001 & orig != 0;
        self.status.update(Flag::C, bit_0_set);

        self.update_zero_and_negative_flags(result);
    }

    /// RTI - Return from Interrupt
    /// The RTI instruction is used at the end of an interrupt processing routine. It pulls the processor flags from the stack followed by the program counter.
    fn rti(&mut self) {
        let restored_status = self.stack_pop();
        let restored_program_counter = self.stack_pop_u16();

        self.status.restore(restored_status);
        self.status.unset(Flag::B);
        self.status.set(Flag::S);
        self.program_counter = restored_program_counter;
    }

    /// RTS - Return from Subroutine
    /// The RTS instruction is used at the end of a subroutine to return to the calling routine. It pulls the program counter (minus one) from the stack.
    fn rts(&mut self) {
        let pc_addr = self.stack_pop_u16();
        self.program_counter = pc_addr.wrapping_add(1);
    }

    /// SBC - Subtract with Carry
    /// This instruction subtracts the contents of a memory location to the accumulator together with the not of the carry bit. If overflow occurs the carry bit is clear, this enables multiple byte subtraction to be performed.
    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    /// SEC - Set Carry Flag
    /// Set the carry flag to one
    fn sec(&mut self) {
        self.status.set(Flag::C);
    }

    /// SED - Set Decimal Flag
    /// Set the decimal flag to one
    fn sed(&mut self) {
        self.status.set(Flag::D);
    }

    /// SEC - Set Interrupt Flag
    /// Set the interrupt disable flag to one
    fn sei(&mut self) {
        self.status.set(Flag::I);
    }

    /// SLO - (Unofficial)
    /// Shift left one bit in memory, then OR accumulator with memory.
    fn slo(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let bit_7_set = data & 0b1000_0000 == 0b1000_0000;

        let result = if self.status.get(Flag::C) { 
            (data << 1) + 1
         } else {
            data << 1
         };
         self.status.update(Flag::C, bit_7_set);

         self.mem_write(addr, result);

         self.register_a = self.register_a | result;
         self.update_zero_and_negative_flags(self.register_a);
    }

    /// SRE - (Unofficial)
    /// Shift right one bit in memory, then EOR accumulator with memory.
    fn sre(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let bit_1_set = data & 0b0000_0001 == 0b0000_0001;
        let result = if self.status.get(Flag::C) {
            (data >> 1) | 0b1000_0000
        } else {
            data >> 1
        };
        self.status.update(Flag::C, bit_1_set);
        self.mem_write(addr, result);

        self.register_a = self.register_a ^ result;
    }

    /// STA - Store Accumulator
    /// Stores the contents of the accumulator into memory.
    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    /// STX - Store X Register
    /// Stores the contents of the X register into memory.
    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    /// STX - Store Y Register
    /// Stores the contents of the Y register into memory.
    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    /// SXA - (Unofficial)
    /// AND X register with the high byte of the target address of the argument + 1.
    fn sxa(&mut self) {
        let addr = self.mem_read_u16(self.program_counter) + self.register_y as u16;
        let data = self.register_x & ((addr >> 8) as u8 + 1);
        self.mem_write(addr, data);
    }

    /// SYA - (Unofficial)
    /// AND Y register with the high byte of the target address of the argument + 1.
    fn sya(&mut self) {
        let addr = self.mem_read_u16(self.program_counter) + self.register_x as u16;
        let data = self.register_y & ((addr >> 8) as u8 + 1);
        self.mem_write(addr, data);
    }

    /// TAX - Transfer Accumulator to X
    /// Copies the current contents of the accumulator into the X register and sets the zero and negative flags as appropriate
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// TAY - Transfer Accumulator to Y
    /// Copies the current contents of the accumulator into the Y register and sets the zero and negative flags as appropriate
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// TOP - (Unofficial)
    ///  Triple NOP
    fn top(&self) {}

    /// TSX - Transfer Stack Pointer to X
    /// Copies the current contents of the stack register into the X register and sets the zero and negative flags as appropriate.
    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// TXA - Transfer X to accumulator
    /// Copies the current contents of the X register into the accumulator and sets the zero and negative flags as appropriate
    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// TXS - Transfer X to Stack Pointer
    /// Copies the current contents of the X register into the stack.
    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }

    /// TYA - Transfer Y to accumulator
    /// Copies the current contents of the Y register into the accumulator and sets the zero and negative flags as appropriate
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// XAA - (Unofficial)
    /// Differing opinions. 
    fn xaa(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let result = self.register_a & self.register_x & data;
        self.update_zero_and_negative_flags(result);

        self.register_a = self.register_a & self.register_x & (data | 0xEF);
    }

    /// XAS - (Unofficial)
    /// AND X register with accumulator and store result in stack pointer, then
    /// AND stack pointer with the high byte of the target address of the
    /// argument + 1. Store result in memory.
    fn xas(&mut self) {
        let addr = self.mem_read_u16(self.program_counter) + self.register_y as u16;

        self.stack_pointer = self.register_a & self.register_x;
        let data = ((addr >> 8) as u8 + 1) & self.stack_pointer;

        self.mem_write(addr, data);
    }

}

/// Helper functions
impl CPU {

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
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

    fn add_to_register_a(&mut self, data: u8) {
        self.register_a = self.add(self.register_a, data);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn add(&mut self, value_a: u8, value_b: u8) -> u8 {
        let sum = value_a as u16 + value_b as u16 + if self.status.get(Flag::C) { 1 } else {0} as u16;
        let carry = sum > 0xff;
        self.status.update(Flag::C, carry);

        let result = sum as u8;
        let overflow = (value_b ^ result) & (result ^ value_a) & 0x80 != 0;
        self.status.update(Flag::V, overflow);

        result
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

}

/// Runtime functions
impl CPU {

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) where F: FnMut(&mut CPU) {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
        loop {
            callback(self);
            // Read from the ROM the byte at the address in the PC
            let code = self.mem_read(self.program_counter);

            // Increment the PC to the next instruction
            self.program_counter += 1;
            let program_counter_state = self.program_counter;
            
            let opcode = opcodes
                .get(&code)
                .expect(&format!("OpCode {:x} is not recognized", code));
            

            match opcode.mnemonic {
                "AAC" => self.aac(&opcode.mode),
                "AAX" => self.aax(&opcode.mode),
                "ARR" => self.arr(&opcode.mode),
                "ASR" => self.asr(&opcode.mode),
                "ATX" => self.atx(&opcode.mode),
                "AXA" => self.axa(&opcode.mode),
                "AXS" => self.axs(&opcode.mode),
                "ADC" => self.adc(&opcode.mode),
                "AND" => self.and(&opcode.mode),
                "ASL" => self.asl(&opcode.mode),
                "BCC" => self.bcc(),
                "BCS" => self.bcs(),
                "BEQ" => self.beq(),
                "BIT" => self.bit(&opcode.mode),
                "BMI" => self.bmi(),
                "BNE" => self.bne(),
                "BPL" => self.bpl(),
                "BVC" => self.bvc(),
                "BVS" => self.bvs(),
                "CLC" => self.clc(),
                "CLD" => self.cld(),
                "CLI" => self.cli(),
                "CLV" => self.clv(),
                "CMP" => self.cmp(&opcode.mode),
                "CPX" => self.cpx(&opcode.mode),
                "CPY" => self.cpy(&opcode.mode),
                "DCP" => self.dcp(&opcode.mode),
                "DEC" => self.dec(&opcode.mode),
                "DEX" => self.dex(),
                "DEY" => self.dey(),
                "DOP" => self.dop(),
                "EOR" => self.eor(&opcode.mode),
                "INC" => self.inc(&opcode.mode),
                "INX" => self.inx(),
                "INY" => self.iny(),
                "ISC" => self.isc(&opcode.mode),
                "JMP" => self.jmp(&opcode.mode),
                "JSR" => self.jsr(),
                "KIL" => self.kil(),
                "LAR" => self.lar(&opcode.mode),
                "LAX" => self.lax(&opcode.mode),
                "LDA" => self.lda(&opcode.mode),
                "LDX" => self.ldx(&opcode.mode),
                "LDY" => self.ldy(&opcode.mode),
                "LSR" => self.lsr(&opcode.mode),
                "NOP" => self.nop(),
                "ORA" => self.ora(&opcode.mode),
                "PHA" => self.pha(),
                "PHP" => self.php(),
                "PLA" => self.pla(),
                "PLP" => self.plp(),
                "RLA" => self.rla(&opcode.mode),
                "ROL" => self.rol(&opcode.mode),
                "ROR" => self.ror(&opcode.mode),
                "RRA" => self.rra(&opcode.mode),
                "RTI" => self.rti(),
                "RTS" => self.rts(),
                "SBC" => self.sbc(&opcode.mode),
                "SEC" => self.sec(),
                "SED" => self.sed(),
                "SEI" => self.sei(),
                "SLO" => self.slo(&opcode.mode),
                "SRE" => self.sre(&opcode.mode),
                "STA" => self.sta(&opcode.mode),
                "STX" => self.stx(&opcode.mode),
                "STY" => self.sty(&opcode.mode),
                "SXA" => self.sxa(),
                "SYA" => self.sya(),
                "TAX" => self.tax(),
                "TSX" => self.tsx(),
                "TAY" => self.tay(),
                "TOP" => self.top(),
                "TXS" => self.txs(),
                "TXA" => self.txa(),
                "TYA" => self.tya(),
                "XAA" => self.xaa(&opcode.mode),
                "XAS" => self.xas(),
                "BRK" => return,
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }
}
