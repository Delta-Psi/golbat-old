use super::asm::op;
use self::op::Op::{self, *};
//use self::op::Reg8::*;
//use self::op::Reg16::*;

use super::{Registers, Mapper};

/// Runs a given operation and returns the CPU cycles.
pub fn run_op<M: Mapper>(rg: &mut Registers, m: &mut M, op: Op) -> Option<u8> {
    match op {
        ld_r_n(r, n) => {
            rg[r] = n;
            Some(8)
        },
        ld_r_r(r1, r2) => {
            rg[r1] = rg[r2];
            Some(4)
        },
        ld_r_iHL(r) => {
            rg[r] = m[rg.get_hl()];
            Some(8)
        },

        _ => unimplemented!(),
    }
}
