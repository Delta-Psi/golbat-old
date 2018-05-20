use cpu::MemoryMap;
use rom::Rom;

#[derive(Debug)]
pub struct Mapper {
    pub rom: Rom,

    pub video_ram: Vec<u8>,
    pub switchable_ram: Vec<u8>,
    pub internal_ram1: Vec<u8>,
    pub oam: Vec<u8>,
    pub internal_ram2: Vec<u8>,
}

impl Mapper {
    pub fn new(rom: Rom) -> Mapper {
        Mapper {
            rom,

            video_ram: vec![0; 0x2000],
            switchable_ram: vec![0; 0x2000],
            internal_ram1: vec![0; 0x2000],
            oam: vec![0; 0xa0],
            internal_ram2: vec![0; 0x7f],
        }
    }
}

impl MemoryMap for Mapper {
    fn read_u8(&self, offset: u16) -> u8 {
        let offset = offset as usize;
        if offset < 0x8000 {
            // TODO: switchable rom banks
            self.rom.data[offset]
        } else if offset < 0xa000 {
            self.video_ram[offset - 0x8000]
        } else if offset < 0xc000 {
            // TODO: switchable ram
            self.switchable_ram[offset - 0xa000]
        } else if offset < 0xe000 {
            self.internal_ram1[offset - 0xc000]
        } else if offset < 0xfe00 {
            self.internal_ram1[offset - 0xe000]
        } else if offset < 0xfea0 {
            self.oam[offset - 0xfea0]
        } else if offset < 0xff00 {
            panic!("invalid memory access at {}", offset)
        } else if offset < 0xff4c {
            unimplemented!("io ports")
        } else if offset < 0xff80 {
            panic!("invalid memory access at {}", offset)
        } else {
            self.internal_ram2[offset - 0xff80]
        }
    }

    fn write_u8(&mut self, offset: u16, value: u8) {
        let offset = offset as usize;
        if offset < 0x8000 {
            // TODO: switchable rom banks
            self.rom.data[offset] = value
        } else if offset < 0xa000 {
            self.video_ram[offset - 0x8000] = value
        } else if offset < 0xc000 {
            // TODO: switchable ram
            self.switchable_ram[offset - 0xa000] = value
        } else if offset < 0xe000 {
            self.internal_ram1[offset - 0xc000] = value
        } else if offset < 0xfe00 {
            self.internal_ram1[offset - 0xe000] = value
        } else if offset < 0xfea0 {
            self.oam[offset - 0xfea0] = value
        } else if offset < 0xff00 {
            panic!("invalid memory access at {}", offset)
        } else if offset < 0xff4c {
            unimplemented!("io ports")
        } else if offset < 0xff80 {
            panic!("invalid memory access at {}", offset)
        } else {
            self.internal_ram2[offset - 0xff80] = value
        }
    }
}
