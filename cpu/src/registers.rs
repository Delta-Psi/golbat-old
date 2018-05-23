use asm::{Reg16, Reg8};
use std::ops::{Index, IndexMut};
use std::fmt;

bitflags! {
    #[derive(Default)]
    pub struct Flags: u8 {
        /// Zero flag
        const Z = 0b1000_0000;
        /// Subtract flag
        const N = 0b0100_0000;
        /// Half carry flag
        const H = 0b0010_0000;
        /// Carry flag
        const C = 0b0001_0000;
    }
}

#[derive(Default)]
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

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Registers")
            .field("af", &format_args!("0x{:02x}{:02x} [{:?}]", self.a, self.f.bits(), self.f))
            .field("bc", &format_args!("0x{:02x}{:02x}", self.b, self.c))
            .field("de", &format_args!("0x{:02x}{:02x}", self.d, self.e))
            .field("hl", &format_args!("0x{:02x}{:02x}", self.h, self.l))
            .field("sp", &format_args!("0x{:04x}", self.sp))
            .field("pc", &format_args!("0x{:04x}", self.pc))
            .finish()
    }
}

impl Registers {
    pub fn get_af(&self) -> u16 {
        (u16::from(self.a) << 8) | u16::from(self.f.bits())
    }

    pub fn set_af(&mut self, af: u16) {
        self.a = ((af & 0xff00) >> 8) as u8;
        let f = (af & 0x00ff) as u8;
        self.f = Flags::from_bits_truncate(f);
    }

    pub fn get_bc(&self) -> u16 {
        (u16::from(self.b) << 8) | u16::from(self.c)
    }

    pub fn set_bc(&mut self, bc: u16) {
        self.b = ((bc & 0xff00) >> 8) as u8;
        self.c = (bc & 0x00ff) as u8;
    }

    pub fn get_de(&self) -> u16 {
        (u16::from(self.d) << 8) | u16::from(self.e)
    }

    pub fn set_de(&mut self, de: u16) {
        self.d = ((de & 0xff00) >> 8) as u8;
        self.e = (de & 0x00ff) as u8;
    }

    pub fn get_hl(&self) -> u16 {
        (u16::from(self.h) << 8) | u16::from(self.l)
    }

    pub fn set_hl(&mut self, hl: u16) {
        self.h = ((hl & 0xff00) >> 8) as u8;
        self.l = (hl & 0x00ff) as u8;
    }
}

impl Index<Reg8> for Registers {
    type Output = u8;

    fn index(&self, index: Reg8) -> &u8 {
        use self::Reg8::*;

        match index {
            A => &self.a,
            B => &self.b,
            C => &self.c,
            D => &self.d,
            E => &self.e,
            H => &self.h,
            L => &self.l,
        }
    }
}

impl IndexMut<Reg8> for Registers {
    fn index_mut(&mut self, index: Reg8) -> &mut u8 {
        use self::Reg8::*;

        match index {
            A => &mut self.a,
            B => &mut self.b,
            C => &mut self.c,
            D => &mut self.d,
            E => &mut self.e,
            H => &mut self.h,
            L => &mut self.l,
        }
    }
}

impl Registers {
    pub fn get_rr(&self, index: Reg16) -> u16 {
        use self::Reg16::*;

        match index {
            AF => self.get_af(),
            BC => self.get_bc(),
            DE => self.get_de(),
            HL => self.get_hl(),
            SP => self.sp,
            PC => self.pc,
        }
    }

    pub fn set_rr(&mut self, index: Reg16, value: u16) {
        use self::Reg16::*;

        match index {
            AF => self.set_af(value),
            BC => self.set_bc(value),
            DE => self.set_de(value),
            HL => self.set_hl(value),
            SP => self.sp = value,
            PC => self.pc = value,
        }
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
