use super::Op;
use super::op::{Reg16, Reg8};
use std::fmt::{Display, Formatter, Result};

impl Display for Reg8 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::Reg8::*;

        write!(
            f,
            "{}",
            match *self {
                A => "A",
                B => "B",
                C => "C",
                D => "D",
                E => "E",
                H => "H",
                L => "L",
            }
        )
    }
}

impl Display for Reg16 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::Reg16::*;

        write!(
            f,
            "{}",
            match *self {
                AF => "AF",
                BC => "BC",
                DE => "DE",
                HL => "HL",
                SP => "SP",
                PC => "PC",
            }
        )
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::Op::*;

        match *self {
            ld_r_n(r, n) => write!(f, "ld {}, ${:02x}", r, n),
            ld_r_r(r1, r2) => write!(f, "ld {}, {}", r1, r2),
            ld_r_iHL(r) => write!(f, "ld {}, (HL)", r),
            ld_iHL_r(r) => write!(f, "ld (HL), {}", r),
            ld_iHL_n(n) => write!(f, "ld (HL), ${:02x}", n),
            ld_A_iBC => write!(f, "ld A, (BC)"),
            ld_A_iDE => write!(f, "ld A, (DE)"),
            ld_A_inn(nn) => write!(f, "ld A, (${:04x})", nn),
            ld_iBC_A => write!(f, "ld (BC), A"),
            ld_iDE_A => write!(f, "ld (DE), A"),
            ld_inn_A(nn) => write!(f, "ld (${:04x}), A", nn),
            ld_A_ioC => write!(f, "ld A, ($ff00+C)"),
            ld_ioC_A => write!(f, "ld ($ff00+C), A"),
            ld_A_ion(n) => write!(f, "ldh A, ($ff00+${:02x})", n),
            ld_ion_A(n) => write!(f, "ldh ($ff00+${:02x}), A", n),
            ldd_A_iHL => write!(f, "ldd A, (HL)"),
            ldd_iHL_A => write!(f, "ldd (HL), A"),
            ldi_A_iHL => write!(f, "ldi A, (HL)"),
            ldi_iHL_A => write!(f, "ldi (HL), A"),

            ld_rr_nn(rr, nn) => write!(f, "ld {}, ${:04x}", rr, nn),
            ld_SP_HL => write!(f, "ld SP, HL"),
            ldhl_SP_n(n) => write!(f, "ldhl SP, ${:04x}", n),
            ld_inn_SP(nn) => write!(f, "ld (${:04x}), SP", nn),
            push(rr) => write!(f, "push {}", rr),
            pop(rr) => write!(f, "push {}", rr),

            add_A_r(r) => write!(f, "add A, {}", r),
            add_A_iHL => write!(f, "add A, (HL)"),
            add_A_n(n) => write!(f, "add A, ${:02x}", n),

            adc_A_r(r) => write!(f, "adc A, {}", r),
            adc_A_iHL => write!(f, "adc A, (HL)"),
            adc_A_n(n) => write!(f, "adc A, ${:02x}", n),

            sub_r(r) => write!(f, "sub {}", r),
            sub_iHL => write!(f, "sub (HL)"),
            sub_n(n) => write!(f, "sub ${:02x}", n),

            sbc_A_r(r) => write!(f, "sbc A, {}", r),
            sbc_A_iHL => write!(f, "sbc A, (HL)"),
            sbc_A_n(n) => write!(f, "sbc A, ${:02x}", n),

            and_r(r) => write!(f, "and {}", r),
            and_iHL => write!(f, "and (HL)"),
            and_n(n) => write!(f, "and ${:02x}", n),

            or_r(r) => write!(f, "or {}", r),
            or_iHL => write!(f, "or (HL)"),
            or_n(n) => write!(f, "or ${:02x}", n),

            xor_r(r) => write!(f, "xor {}", r),
            xor_iHL => write!(f, "xor (HL)"),
            xor_n(n) => write!(f, "xor ${:02x}", n),

            cp_r(r) => write!(f, "cp {}", r),
            cp_iHL => write!(f, "cp (HL)"),
            cp_n(n) => write!(f, "cp ${:02x}", n),

            inc_r(r) => write!(f, "inc {}", r),
            inc_iHL => write!(f, "inc (HL)"),
            
            dec_r(r) => write!(f, "dec {}", r),
            dec_iHL => write!(f, "dec (HL)"),
            
            add_HL_rr(rr) => write!(f, "add HL, {}", rr),
            add_SP_n(n) => write!(f, "add SP, ${:02x}", n),
            
            inc_rr(rr) => write!(f, "inc {}", rr),
            dec_rr(rr) => write!(f, "dec {}", rr),

            swap_r(r) => write!(f, "swap {}", r),
            swap_iHL => write!(f, "swap (HL)"),

            daa => write!(f, "daa"),
            cpl => write!(f, "cpl"),
            ccf => write!(f, "ccf"),
            scf => write!(f, "scf"),
            nop => write!(f, "nop"),
            halt => write!(f, "halt"),
            stop => write!(f, "stop"),
            di => write!(f, "di"),
            ei => write!(f, "ei"),

            rlca => write!(f, "rlca"),
            rla => write!(f, "rla"),
            rrca => write!(f, "rrca"),
            rra => write!(f, "rra"),

            rlc_r(r) => write!(f, "rlc {}", r),
            rlc_iHL => write!(f, "rlc (HL)"),

            rl_r(r) => write!(f, "rl {}", r),
            rl_iHL => write!(f, "rl (HL)"),

            rrc_r(r) => write!(f, "rrc {}", r),
            rrc_iHL => write!(f, "rrc (HL)"),

            rr_r(r) => write!(f, "rr {}", r),
            rr_iHL => write!(f, "rr (HL)"),

            sla_r(r) => write!(f, "sla {}", r),
            sla_iHL => write!(f, "sla (HL)"),

            sra_r(r) => write!(f, "sra {}", r),
            sra_iHL => write!(f, "sra (HL)"),

            srl_r(r) => write!(f, "srl {}", r),
            srl_iHL => write!(f, "srl (HL)"),

            bit_r(b, r) => write!(f, "bit {}, {}", b, r),
            bit_iHL(b) => write!(f, "bit {}, (HL)", b),

            set_r(b, r) => write!(f, "set {}, {}", b, r),
            set_iHL(b) => write!(f, "set {}, (HL)", b),

            res_r(b, r) => write!(f, "res {}, {}", b, r),
            res_iHL(b) => write!(f, "res {}, (HL)", b),

            jp(nn) => write!(f, "jp ${:04x}", nn),
            jp_NZ(nn) => write!(f, "jp NZ, ${:04x}", nn),
            jp_Z(nn) => write!(f, "jp Z, ${:04x}", nn),
            jp_NC(nn) => write!(f, "jp NC, ${:04x}", nn),
            jp_C(nn) => write!(f, "jp C, ${:04x}", nn),
            jp_iHL => write!(f, "jp (HL)"),

            jr(n) => write!(f, "jr ${:02x}", n),
            jr_NZ(n) => write!(f, "jr NZ, ${:02x}", n),
            jr_Z(n) => write!(f, "jr Z, ${:02x}", n),
            jr_NC(n) => write!(f, "jr NC, ${:02x}", n),
            jr_C(n) => write!(f, "jr C, ${:02x}", n),
            
            call(nn) => write!(f, "call ${:04x}", nn),
            call_NZ(nn) => write!(f, "call NZ, ${:04x}", nn),
            call_Z(nn) => write!(f, "call Z, ${:04x}", nn),
            call_NC(nn) => write!(f, "call NC, ${:04x}", nn),
            call_C(nn) => write!(f, "call C, ${:04x}", nn),

            _ => unimplemented!(),
        }
    }
}
