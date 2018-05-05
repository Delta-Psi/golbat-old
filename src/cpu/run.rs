use super::asm::op;
use self::op::Op::{self, *};
//use self::op::Reg8::*;
//use self::op::Reg16::*;

use super::{Registers, Mapper};

/// Runs a given operation and returns the CPU cycles.
pub fn run_op<M: Mapper>(rg: &mut Registers, m: &mut M, op: Op) -> Option<u8> {
    match op {
        // load commands
        // 8-bit value to register
        ld_r_n(r, n) => {
            rg[r] = n;
            Some(8)
        },
        // 8-bit register to register
        ld_r_r(r1, r2) => {
            rg[r1] = rg[r2];
            Some(4)
        },
        // address at HL to 8-bit register
        ld_r_iHL(r) => {
            rg[r] = m.read_u8(rg.get_hl());
            Some(8)
        },
        // 8-bit register to address at HL
        ld_iHL_r(r) => {
            m.write_u8(tg.get_hl(), rg[r]);
            Some(8)
        },
        // 8-bit value to address at HL
        ld_iHL_n(n) => {
            m.write_u8(tg.get_hl(), n);
            Some(12)
        },

        _ => unimplemented!(),
    }
}
