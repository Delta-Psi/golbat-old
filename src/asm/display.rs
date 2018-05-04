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
            ld_r_n(r, n) =>
                write!(f, "ld {}, ${:02x}", r, n),
            ld_r_r(r1, r2) =>
                write!(f, "ld {}, {}", r1, r2),
            ld_r_iHL(r) =>
                write!(f, "ld {}, (HL)", r),
            ld_iHL_r(r) =>
                write!(f, "ld (HL), {}", r),
            ld_iHL_n(n) =>
                write!(f, "ld (HL), ${:02x}", n),
            ld_A_iBC =>
                write!(f, "ld A, (BC)"),
            ld_A_iDE =>
                write!(f, "ld A, (DE)"),
            ld_A_inn(nn) =>
                write!(f, "ld A, (${:04x})", nn),
            ld_iBC_A =>
                write!(f, "ld (BC), A"),
            ld_iDE_A =>
                write!(f, "ld (DE), A"),
            ld_inn_A(nn) =>
                write!(f, "ld (${:04x}), A", nn),
            ld_A_ioC =>
                write!(f, "ld A, (C)"),
            ld_ioC_A =>
                write!(f, "ld (C), A"),
            ld_A_ion(n) =>
                write!(f, "ldh A, (${:02x})", n),
            ld_ion_A(n) =>
                write!(f, "ldh (${:02x}), A", n),
            ldd_A_iHL =>
                write!(f, "ldd A, (HL)"),
            ldd_iHL_A =>
                write!(f, "ldd (HL), A"),
            ldi_A_iHL =>
                write!(f, "ldi A, (HL)"),
            ldi_iHL_A =>
                write!(f, "ldi (HL), A"),
            
            ld_rr_nn(rr, nn) =>
                write!(f, "ld {}, ${:04x}", rr, nn),
            ld_SP_HL =>
                write!(f, "ld SP, HL"),
            ldhl_SP_n(n) =>
                write!(f, "ldhl SP, ${:04x}", n),

            _ => unimplemented!(),
        }
    }
}
