const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 16384;
const CHR_ROM_PAGE_SIZE: usize = 8192;

#[derive(Debug, PartialEq)]
pub enum Mirroring {
    VERTICAL,
    HORIZONTAL,
    FOUR_SCREEN,
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
}

impl Rom {
    pub fn new(raw: &Vec<u8>) -> Result<Rom, String> {
        if &raw[0..4] != NES_TAG {
            return Err("File is not in iNES format.".to_string());
        }
        let mapper = extract_mapper(raw[6], raw[7]);
        let ines_ver = extract_ines_ver(raw[7]);
        if ines_ver != 0 {
            return Err("NES 2.0 format not supported.".to_string());
        }
        let screen_mirroring = extract_mirroring(raw[6]);

        let prg_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;

        let skip_trainer = extract_skip_trainer(raw[6]);

        let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        println!(
            "Constructed ROM from raw file with PRG size {}, CHR size {}, and mapper {}.",
            prg_rom_size, chr_rom_size, mapper
        );

        Ok(Rom {
            prg_rom: raw[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: raw[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
            mapper,
            screen_mirroring,
        })
    }

    pub fn generate_standalone_rom(program: Vec<u8>) -> Self {
        let mut prg_rom = vec![0; 0x4000];

        for i in 0..(program.len() as u16) {
            prg_rom[i as usize] = program[i as usize];
        }

        //Provides the starting memory address - 0xfffc once connected
        prg_rom[0x3ffc] = 0x00;
        prg_rom[0x3ffd] = 0x80;

        Rom {
            prg_rom,
            chr_rom: vec![0; 0x2000],
            mapper: 0,
            screen_mirroring: Mirroring::HORIZONTAL,
        }
    }
}

fn extract_mirroring(byte_6: u8) -> Mirroring {
    let four_screen = byte_6 & 0b1000 != 0;
    let vertical_mirroring = byte_6 & 0b1 != 0;
    match (four_screen, vertical_mirroring) {
        (true, _) => Mirroring::FOUR_SCREEN,
        (false, true) => Mirroring::VERTICAL,
        (false, false) => Mirroring::HORIZONTAL,
    }
}

fn extract_mapper(byte_6: u8, byte_7: u8) -> u8 {
    (byte_7 & 0b1111_0000) | (byte_6 >> 4)
}

fn extract_ines_ver(byte_7: u8) -> u8 {
    (byte_7 >> 2) & 0b11
}

fn extract_skip_trainer(byte_6: u8) -> bool {
    byte_6 & 0b100 != 0
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    #[should_panic]
    fn panics_without_ines_tag() {
        Rom::new(&vec![0x45, 0x45, 0x45, 0x45]).unwrap();
    }

    #[test]
    fn gets_correct_screen_mirroring() {
        assert_eq!(extract_mirroring(0b1001), Mirroring::FOUR_SCREEN);
        assert_eq!(extract_mirroring(0b0001), Mirroring::VERTICAL);
        assert_eq!(extract_mirroring(0b0000), Mirroring::HORIZONTAL);
    }
}
