use bitflags::bitflags;

bitflags! {
    /// 7  bit  0
    /// ---- ----
    /// VSO. ....
    /// |||| ||||
    /// |||+-++++- PPU open bus. Returns stale PPU bus contents.
    /// ||+------- Sprite overflow. The intent was for this flag to be set
    /// ||         whenever more than eight sprites appear on a scanline, but a
    /// ||         hardware bug causes the actual behavior to be more complicated
    /// ||         and generate false positives as well as false negatives; see
    /// ||         PPU sprite evaluation. This flag is set during sprite
    /// ||         evaluation and cleared at dot 1 (the second dot) of the
    /// ||         pre-render line.
    /// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    /// |          a nonzero background pixel; cleared at dot 1 of the pre-render
    /// |          line.  Used for raster timing.
    /// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
    ///            Set at dot 1 of line 241 (the line *after* the post-render
    ///            line); cleared after reading $2002 and at dot 1 of the
    ///            pre-render line.
    ///
    pub struct PPUStatusRegister: u8 {
        const OPEN_0 = 0b0000_0001;
        const OPEN_1 = 0b0000_0010;
        const OPEN_2 = 0b0000_0100;
        const OPEN_3 = 0b0000_1000;
        const OPEN_4 = 0b0001_0000;
        const SPRITE_OVERFLOW = 0b0010_0000;
        const SPRITE_0_HIT = 0b0100_0000;
        const VBLANK = 0b1000_0000;
    }
}

impl PPUStatusRegister {
    pub fn new() -> Self {
        PPUStatusRegister::from_bits_truncate(0b0000_0000)
    }

    pub fn update(&mut self, data: u8) {
        self.bits = data;
    }

    pub fn set_vblank_status(&mut self, status: bool) {
        self.set(PPUStatusRegister::VBLANK, status);
    }

    pub fn reset_vblank_status(&mut self) {
        self.remove(PPUStatusRegister::VBLANK);
    }

    pub fn is_in_vblank(&self) -> bool {
        self.contains(PPUStatusRegister::VBLANK)
    }
}
