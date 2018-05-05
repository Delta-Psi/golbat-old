pub mod asm;

bitflags! {
    #[derive(Default)]
    pub struct Flags: u8 {
        /// Zero flag
        const Z = 0b1000_0000;
        /// Subtract flg
        const N = 0b0100_0000;
        /// Half carry flag
        const H = 0b0010_0000;
        /// Carry flag
        const C = 0b0001_0000;
    }
}

#[derive(Debug, Default)]
pub struct Registers {
    pub a: u8,
    pub f: Flags,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
}

impl Registers {
    pub fn get_af(&self) -> u16 {
        ((self.a as u16) << 8) | self.f.bits() as u16
    }

    pub fn set_af(&mut self, af: u16) {
        self.a = ((af & 0xff00) >> 8) as u8;
        let f = (af & 0x00ff) as u8;
        self.f = Flags::from_bits_truncate(f);
    }

    pub fn get_bc(&self) -> u16 {
        ((self.b as u16) << 8) | self.c as u16
    }

    pub fn set_bc(&mut self, bc: u16) {
        self.b = ((bc & 0xff00) >> 8) as u8;
        self.c = (bc & 0x00ff) as u8;
    }

    pub fn get_de(&self) -> u16 {
        ((self.d as u16) << 8) | self.e as u16
    }

    pub fn set_de(&mut self, de: u16) {
        self.d = ((de & 0xff00) >> 8) as u8;
        self.e = (de & 0x00ff) as u8;
    }

    pub fn get_hl(&self) -> u16 {
        ((self.h as u16) << 8) | self.l as u16
    }

    pub fn set_hl(&mut self, hl: u16) {
        self.h = ((hl & 0xff00) >> 8) as u8;
        self.l = (hl & 0x00ff) as u8;
    }
}

#[test]
fn registers() {
    let mut registers = Registers::default();

    assert_eq!(registers.get_af(), 0x0000);
    registers.set_af(0x6180);
    assert_eq!(registers.get_af(), 0x6180);
    assert_eq!(registers.a, 0x61);
    assert_eq!(registers.f.bits(), 0x80);
    registers.a = 0x11;
    assert_eq!(registers.get_af(), 0x1180);

    assert_eq!(registers.get_bc(), 0x0000);
    registers.set_bc(0x5314);
    assert_eq!(registers.get_bc(), 0x5314);
    assert_eq!(registers.b, 0x53);
    assert_eq!(registers.c, 0x14);
    registers.c = 0x72;
    assert_eq!(registers.get_bc(), 0x5372);

    assert_eq!(registers.get_de(), 0x0000);
    registers.set_de(0xae17);
    assert_eq!(registers.get_de(), 0xae17);
    assert_eq!(registers.d, 0xae);
    assert_eq!(registers.e, 0x17);
    registers.d = 0xaa;
    assert_eq!(registers.get_de(), 0xaa17);

    assert_eq!(registers.get_hl(), 0x0000);
    registers.set_hl(0xbeef);
    assert_eq!(registers.get_hl(), 0xbeef);
    assert_eq!(registers.h, 0xbe);
    assert_eq!(registers.l, 0xef);
    registers.l = 0xb0;
    assert_eq!(registers.get_hl(), 0xbeb0);
}

