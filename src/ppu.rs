pub mod addr_register;
pub mod control_register;
pub mod mask_register;
pub mod scroll_register;
pub mod status_register;

use crate::rom::Mirroring;

use self::addr_register::AddrRegister;
use self::control_register::ControlRegister;
use self::mask_register::MaskRegister;
use self::scroll_register::ScrollRegister;
use self::status_register::PPUStatusRegister;

pub struct NesPPU {
    pub chr_rom: Vec<u8>,        // visuals of a game stored on a cartridge
    pub palette_table: [u8; 32], // internal memory to keep palettes for use on the screen
    pub vram: [u8; 2048],        // 2kb of blank space to hold background information
    pub oam_data: [u8; 256],     // internal memory to keep state of sprites
    pub addr: AddrRegister,
    pub ctrl: ControlRegister,
    pub internal_data_buffer: u8,
    pub mask_register: MaskRegister,
    pub status_register: PPUStatusRegister,
    pub scroll_register: ScrollRegister,
    pub oam_addr: u8,
    pub mirroring: Mirroring,
    scanline: u16,
    cycles: usize,
    pub nmi_interrupt: Option<u8>,
}

pub trait PPU {
    fn write_to_ctrl(&mut self, value: u8);
    fn write_to_mask(&mut self, value: u8);
    fn read_status(&mut self) -> u8;
    fn write_to_oam_addr(&mut self, value: u8);
    fn write_to_oam_data(&mut self, value: u8);
    fn read_oam_data(&self) -> u8;
    fn write_to_scroll(&mut self, value: u8);
    fn write_to_ppu_addr(&mut self, value: u8);
    fn write_to_data(&mut self, value: u8);
    fn read_data(&mut self) -> u8;
    fn write_oam_dma(&mut self, value: &[u8; 256]);
}

impl NesPPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        NesPPU {
            chr_rom,
            mirroring,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 64 * 4],
            addr: AddrRegister::new(),
            ctrl: ControlRegister::new(),
            internal_data_buffer: 0,
            mask_register: MaskRegister::new(),
            status_register: PPUStatusRegister::new(),
            scroll_register: ScrollRegister::new(),
            oam_addr: 0,
            scanline: 0,
            cycles: 0,
            nmi_interrupt: None,
        }
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_add_increment());
    }

    pub fn mirror_vram_addr(&mut self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0b0010_1111_1111_1111;
        let vram_index = mirrored_vram - 0x2000;
        let name_table = vram_index / 0x0400;
        match (&self.mirroring, name_table) {
            (Mirroring::VERTICAL, 2) | (Mirroring::VERTICAL, 3) => vram_index - 0x0800,
            (Mirroring::HORIZONTAL, 2) => vram_index - 0x0400,
            (Mirroring::HORIZONTAL, 1) => vram_index - 0x0400,
            (Mirroring::HORIZONTAL, 3) => vram_index - 0x0800,
            _ => vram_index,
        }
    }

    pub fn tick(&mut self, cycles: u8) -> bool {
        self.cycles += cycles as usize;
        if self.cycles >= 341 {
            self.scanline += 1;
            self.cycles -= 341;

            if self.scanline == 241 {
                if self.ctrl.generate_vblank_nmi() {
                    self.status_register.set_vblank_status(true);
                    todo!("should trigger nmi interrupt")
                }
            }

            if self.scanline >= 262 {
                self.scanline = 0;
                self.status_register.reset_vblank_status();
                return true;
            }
        }
        return false;
    }
}

impl PPU for NesPPU {
    fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    fn write_to_ctrl(&mut self, data: u8) {
        let before_nmi_status = self.ctrl.generate_vblank_nmi();
        self.ctrl.update(data);
        if !before_nmi_status
            && self.ctrl.generate_vblank_nmi()
            && self.status_register.is_in_vblank()
        {
            self.nmi_interrupt = Some(1);
        }
    }

    fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let result = self.internal_data_buffer;
                self.internal_data_buffer = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x2fff => {
                let result = self.internal_data_buffer;
                self.internal_data_buffer = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            0x3000..=0x3eff => panic!("Attempt to read from invalid address space."),
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3f00) as usize]
            }
            0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize],
            _ => panic!("Unexpected access to mirrored space at {:#x}.", addr),
        }
    }

    fn write_to_data(&mut self, data: u8) {
        let addr = self.addr.get();
        match addr {
            0..=0x1fff => {
                panic!("Attempt to write the CHR ROM at address {}", addr);
            }
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = data;
            }
            0x3000..=0x3eff => panic!("Attempt to write to invalid PPU address at {}.", addr),
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3f00) as usize] = data;
            }
            0x3f00..=0x3fff => {
                self.palette_table[(addr - 0x3f00) as usize] = data;
            }
            _ => panic!("Attempt to write to invalid PPU address at {}.", addr),
        }
        self.increment_vram_addr();
    }

    fn write_to_mask(&mut self, value: u8) {
        self.mask_register.update(value);
    }

    fn read_status(&mut self) -> u8 {
        let data = self.status_register.bits();
        self.addr.reset_latch();
        self.scroll_register.reset_latch();
        self.status_register.remove(PPUStatusRegister::VBLANK);
        data
    }

    fn write_to_oam_addr(&mut self, value: u8) {
        self.oam_addr = value;
    }

    fn write_to_oam_data(&mut self, value: u8) {
        self.oam_data[self.oam_addr as usize] = value;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn read_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    fn write_to_scroll(&mut self, value: u8) {
        self.scroll_register.write(value);
    }

    fn write_oam_dma(&mut self, value: &[u8; 256]) {
        for x in value.iter() {
            self.write_to_oam_data(*x);
        }
    }
}

#[cfg(test)]
mod ppu_tests {

    use super::*;

    fn test_setup() -> NesPPU {
        NesPPU::new(vec![0; 2056], Mirroring::HORIZONTAL)
    }

    #[test]
    fn test_ppu_vram_writes() {
        let mut ppu = test_setup();

        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);
        ppu.write_to_data(0x66);

        assert_eq!(ppu.vram[0x0305], 0x66);
    }

    #[test]
    fn test_cpu_read_from_data() {
        let mut ppu = test_setup();
        ppu.vram[0x0305] = 0xfe;

        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);
        ppu.read_data(); // dummy read
        let read = ppu.read_data();

        assert_eq!(read, 0xfe);
    }

    #[test]
    fn test_ppu_vram_reads_cross_page() {
        let mut ppu = test_setup();

        ppu.write_to_ctrl(0);
        ppu.vram[0x01ff] = 0x66;
        ppu.vram[0x0200] = 0x77;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0xff);

        ppu.read_data(); //dummy read
        assert_eq!(ppu.read_data(), 0x66);
        assert_eq!(ppu.read_data(), 0x77);
    }

    #[test]
    fn test_ppu_vram_reads_step_32() {
        let mut ppu = test_setup();

        ppu.write_to_ctrl(0b100);
        ppu.vram[0x01ff] = 0x66;
        ppu.vram[0x01ff + 32] = 0x77;
        ppu.vram[0x01ff + 64] = 0x88;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0xff);

        ppu.read_data(); //load_into_buffer
        assert_eq!(ppu.read_data(), 0x66);
        assert_eq!(ppu.read_data(), 0x77);
        assert_eq!(ppu.read_data(), 0x88);
    }

    #[test]
    fn test_vram_horizontal_mirror() {
        let mut ppu = test_setup();

        ppu.write_to_ppu_addr(0x24);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_to_data(0x66); //write to a

        ppu.write_to_ppu_addr(0x28);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_to_data(0x77); //write to B

        ppu.write_to_ppu_addr(0x20);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x66); //read from A

        ppu.write_to_ppu_addr(0x2C);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x77); //read from b
    }

    #[test]
    fn test_vram_vertical_mirror() {
        let mut ppu = NesPPU::new(vec![0; 2048], Mirroring::VERTICAL);

        ppu.write_to_ppu_addr(0x20);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_to_data(0x66); //write to A

        ppu.write_to_ppu_addr(0x2C);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_to_data(0x77); //write to b

        ppu.write_to_ppu_addr(0x28);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x66); //read from a

        ppu.write_to_ppu_addr(0x24);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x77); //read from B
    }

    #[test]
    fn read_status_resets_latch() {
        let mut ppu = test_setup();
        ppu.vram[0x0305] = 0x35;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data();
        assert_ne!(ppu.read_data(), 0x35);

        ppu.read_status();

        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data();
        assert_eq!(ppu.read_data(), 0x35);
    }

    #[test]
    fn test_ppu_vram_mirroring() {
        let mut ppu = test_setup();

        ppu.write_to_ctrl(0);
        ppu.vram[0x0305] = 0x66;

        ppu.write_to_ppu_addr(0x63); //0x6305 -> 0x2305
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data();
        assert_eq!(ppu.read_data(), 0x66);
    }

    #[test]
    fn test_read_status_resets_vblank() {
        let mut ppu = test_setup();
        ppu.status_register.insert(PPUStatusRegister::VBLANK);

        let status = ppu.read_status();

        assert_eq!(status >> 7, 1);
        assert_eq!(ppu.status_register.bits() >> 7, 0);
    }

    #[test]
    fn test_oam_read_write() {
        let mut ppu = test_setup();
        ppu.write_to_oam_addr(0x10);
        ppu.write_to_oam_data(0x66);
        ppu.write_to_oam_data(0x77);

        ppu.write_to_oam_addr(0x10);
        assert_eq!(ppu.read_oam_data(), 0x66);

        ppu.write_to_oam_addr(0x11);
        assert_eq!(ppu.read_oam_data(), 0x77);
    }

    #[test]
    fn test_oam_dma() {
        let mut ppu = test_setup();

        let mut data = [0x66; 256];
        data[0] = 0x77;
        data[255] = 0x88;

        ppu.write_to_oam_addr(0x10);
        ppu.write_oam_dma(&data);

        ppu.write_to_oam_addr(0xf); //wrap around
        assert_eq!(ppu.read_oam_data(), 0x88);

        ppu.write_to_oam_addr(0x10);
        assert_eq!(ppu.read_oam_data(), 0x77);

        ppu.write_to_oam_addr(0x11);
        assert_eq!(ppu.read_oam_data(), 0x66);
    }
}
