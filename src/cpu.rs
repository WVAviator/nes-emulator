#[cfg(test)]
mod test;

pub struct CPU {
    pub register_a: u8, // accumulator - stores results of arithmetic and logic operations
    pub register_x: u8, // register x - temporarily stores values
    pub status: u8,     // a register with 7 flags indicating cpu status after each instruction
    pub program_counter: u16, // program counter holds two bytes, points to next instructions
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    // NES uses little Endian addressing rather than big endian - must switch the bytes when reading/writing
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

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = 0;

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
    fn lda(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // TAX - Transfer Accumulator to X
    // Copies the current contents of the accumulator into the X register and sets the zero and negative flags as appropriate
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    // INX - Increment X Register
    // Adds one to the X register setting the zero and negative flags as appropriate
    fn inx(&mut self) {
        if self.register_x == 0xff {
            self.register_x = 0;
        } else {
            self.register_x += 1;
        }
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        // If A = 0, set the Zero Flag
        if result == 0 {
            // Only changes the Zero Flag to 1
            self.status = self.status | 0b0000_0010;
        } else {
            // Only changes the Zero Flag to 0
            self.status = self.status & 0b1111_1101;
        }

        // If bit 7 of result is set, set the Negative Flag
        if result & 0b1000_0000 != 0 {
            // Only changes the Negative Flag to 1
            self.status = self.status | 0b1000_0000;
        } else {
            // Only changes the Negative Flag to 0
            self.status = self.status & 0b0111_1111;
        }
    }

    pub fn run(&mut self) {
        // The CPU works in a constant cycle:

        // Fetch next execution instruction from the instruction memory
        // Decode the instruction
        // Execute the Instruction
        // Repeat the cycle

        loop {
            // Read from the ROM the byte at the address in the PC
            let opscode = self.mem_read(self.program_counter);
            // Increment the PC to the next instruction
            self.program_counter += 1;

            match opscode {
                0xA9 => {
                    let param = self.mem_read(self.program_counter);
                    self.program_counter += 1;
                    self.lda(param);
                }
                0xAA => self.tax(),
                0xE8 => self.inx(),
                0x00 => return,
                _ => todo!(),
            }
        }
    }
}
