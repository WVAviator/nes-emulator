use bitflags::bitflags;

bitflags! {
    ///     7  bit  0
    /// ---- ----
    /// BGRs bMmG
    /// |||| ||||
    /// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
    /// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    /// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    /// |||| +---- 1: Show background
    /// |||+------ 1: Show sprites
    /// ||+------- Emphasize red (green on PAL/Dendy)
    /// |+-------- Emphasize green (red on PAL/Dendy)
    /// +--------- Emphasize blue
    ///
    pub struct MaskRegister: u8 {
        const GREYSCALE = 0b0000_0001;
        const LEFTMOST_8PX_BG = 0b0000_0010;
        const LEFTMOST_8PX_SPRITES = 0b0000_0100;
        const BACKGROUND = 0b0000_1000;
        const SPRITES = 0b0001_0000;
        const EMPHASIZE_RED = 0b0010_0000;
        const EMPHASIZE_GREEN = 0b0100_0000;
        const EMPHASIZE_BLUE = 0b1000_0000;
    }
}

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister::from_bits_truncate(0b0000_0000)
    }

    pub fn update(&mut self, data: u8) {
        self.bits = data;
    }
}
