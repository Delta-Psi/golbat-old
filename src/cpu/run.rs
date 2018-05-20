use super::asm;
use self::asm::Op::{self, *};
// use self::asm::Reg8::*;
use self::asm::Reg16::*;

use super::{Flags, MemoryMap, Registers};

use std::ops::*;

/// Runs a given operation and returns the CPU cycles.
pub fn run_op<M: MemoryMap>(rg: &mut Registers, m: &mut M, op: Op) -> Option<u8> {
    match op {
        // load commands
        // 8-bit value to register
        ld_r_n(r, n) => {
            rg[r] = n;
            Some(8)
        }
        // 8-bit register to register
        ld_r_r(r1, r2) => {
            rg[r1] = rg[r2];
            Some(4)
        }
        // address at HL to 8-bit register
        ld_r_iHL(r) => {
            rg[r] = m.read_u8(rg.get_hl());
            Some(8)
        }
        // 8-bit register to address at HL
        ld_iHL_r(r) => {
            m.write_u8(rg.get_hl(), rg[r]);
            Some(8)
        }
        // 8-bit value to address at HL
        ld_iHL_n(n) => {
            m.write_u8(rg.get_hl(), n);
            Some(12)
        }

        // address at HL to A, decrement HL
        ldd_A_iHL => {
            let hl = rg.get_hl();
            rg.a = m.read_u8(hl);
            rg.set_hl(hl - 1);
            Some(8)
        }
        // A to address at HL, decrement HL
        ldd_iHL_A => {
            let hl = rg.get_hl();
            m.write_u8(hl, rg.a);
            rg.set_hl(hl - 1);
            Some(8)
        }
        // address at HL to A, increment HL
        ldi_A_iHL => {
            let hl = rg.get_hl();
            rg.a = m.read_u8(hl);
            rg.set_hl(hl + 1);
            Some(8)
        }
        // A to address at HL, increment HL
        ldi_iHL_A => {
            let hl = rg.get_hl();
            m.write_u8(hl, rg.a);
            rg.set_hl(hl + 1);
            Some(8)
        }

        // 16-bit value to register
        ld_rr_nn(rr, nn) => {
            match rr {
                BC => rg.set_bc(nn),
                DE => rg.set_de(nn),
                HL => rg.set_hl(nn),
                SP => rg.sp = nn,
                _ => return None,
            };
            Some(12)
        }

        // arithmetical-logical commands
        // xor 8-bit register with A
        xor_r(r) => {
            rg.a = rg.a.bitxor(rg[r]);
            rg.f = Flags::empty();
            check_z(&mut rg.f, &rg.a);
            Some(4)
        }
        xor_iHL => {
            rg.a = rg.a.bitxor(m.read_u8(rg.get_hl()));
            rg.f = Flags::empty();
            check_z(&mut rg.f, &rg.a);
            Some(8)
        }
        xor_n(n) => {
            rg.a = rg.a.bitxor(n);
            rg.f = Flags::empty();
            check_z(&mut rg.f, &rg.a);
            Some(8)
        }

        // misc commands
        // nop
        nop => Some(4),

        // jumps
        // unconditional jump
        jp(nn) => {
            rg.pc = nn;
            Some(12)
        }

        op => unimplemented!("{}", op),
    }
}

fn check_z(f: &mut Flags, value: &u8) {
    if *value == 0 {
        f.insert(Flags::Z);
    }
}
