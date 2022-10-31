/// The PPU Address Register represents CPU memory location 0x2006 and serves as a register for the PPU.
/// When the CPU wants to request information from the PPU's RAM or ROM, it will write the requested address
/// to the PPU Address Register.
/// It will write to the register twice to convey a 2-byte address, but the order is _not_ little endian.
/// Afterwards, the CPU can request the returned data via the PPU Data Register (0x2007)
pub struct AddrRegister {
    value: (u8, u8),
    hi_ptr: bool,
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister {
            value: (0, 0),
            hi_ptr: true,
        }
    }

    fn set(&mut self, data: u16) {
        self.value.0 = (data >> 8) as u8;
        self.value.1 = (data & 0xff) as u8;
    }

    /// This simply updates the address register and will internally set whether the byte received is hi or lo.
    /// To interpret whether this is the first or second byte of an address, the bool hi_ptr is toggled with each read.
    ///
    pub fn update(&mut self, data: u8) {
        if self.hi_ptr {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }

        if self.get() > 0x3fff {
            self.set(self.get() & 0x3fff)
        }
        self.hi_ptr = !self.hi_ptr;
    }

    /// When the PPU writes to its Data Register (0x2007), the Address Register is incremented either by 1 or by 32.
    /// The value of the increment is determined by the VRAM_ADD_INCREMENT flag set in the Control Register.
    pub fn increment(&mut self, inc: u8) {
        let lo = self.value.1;
        self.value.1 = self.value.1.wrapping_add(inc);
        if lo > self.value.1 {
            self.value.0 = self.value.0.wrapping_add(1);
        }
        if self.get() > 0x3fff {
            self.set(self.get() & 0x3fff)
        }
    }

    pub fn reset_latch(&mut self) {
        self.hi_ptr = true;
    }

    pub fn get(&self) -> u16 {
        ((self.value.0 as u16) << 8) | (self.value.1 as u16)
    }
}
