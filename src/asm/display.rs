use super::Op;
use super::op::{Reg8, Reg16};
use std::fmt::{Display, Formatter, Result};

impl Display for Reg8 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::Reg8::*;

        write!(f, "{}", match *self {
            A => "A",
            B => "B",
            C => "C",
            D => "D",
            E => "E",
            H => "H",
            L => "L",
        })
    }
}

impl Display for Reg16 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::Reg16::*;

        write!(f, "{}", match *self {
            AF => "AF",
            BC => "BC",
            DE => "DE",
            HL => "HL",
            SP => "SP",
            PC => "PC",
        })
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::Op::*;

        match *self {
            ld_r_n(ref r, n) =>
                write!(f, "ld {}, ${:x}", r, n),
            ld_r_r(ref r1, ref r2) =>
                write!(f, "ld {}, {}", r1, r2),
            ld_r_iHL(ref r) =>
                write!(f, "ld {}, (HL)", r),

            _ => unimplemented!(),
        }
    }
}
