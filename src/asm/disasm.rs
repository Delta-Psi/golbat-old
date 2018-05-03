//! Gameboy dissasembly tools.

use super::op::*;
use nom::*;

use self::Op::*;
use self::Reg8::*;
use self::Reg16::*;

pub fn parse_op(input: &[u8]) -> IResult<&[u8], Op, u32> {
    let (input, opcode) = try_parse!(input, le_u8);

    let (input, result) = match opcode {
        // 8-bit load commands
        // ld B, n
        0x06 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_r_n(B, value))
        },
        // ld C, n
        0x0E => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_r_n(C, value))
        },
        // ld D, n
        0x16 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_r_n(D, value))
        },
        // ld E, n
        0x1E => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_r_n(E, value))
        },
        // ld H, n
        0x26 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_r_n(H, value))
        },
        // ld L, n
        0x2E => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_r_n(L, value))
        },

        // ld A, A
        0x7F => (input, ld_r_r(A, A)),
        // ld A, B
        0x78 => (input, ld_r_r(A, B)),
        // ld A, C
        0x79 => (input, ld_r_r(A, C)),
        // ld A, D
        0x7A => (input, ld_r_r(A, D)),
        // ld A, E
        0x7B => (input, ld_r_r(A, E)),
        // ld A, H
        0x7C => (input, ld_r_r(A, H)),
        // ld A, L
        0x7D => (input, ld_r_r(A, L)),
        // ld A, (HL)
        0x7E => (input, ld_r_iHL(A)),
        
        // ld B, A
        0x47 => (input, ld_r_r(B, A)),
        // ld B, B
        0x40 => (input, ld_r_r(B, B)),
        // ld B, C
        0x41 => (input, ld_r_r(B, C)),
        // ld B, D
        0x42 => (input, ld_r_r(B, D)),
        // ld B, E
        0x43 => (input, ld_r_r(B, E)),
        // ld B, H
        0x44 => (input, ld_r_r(B, H)),
        // ld B, L
        0x45 => (input, ld_r_r(B, L)),
        // ld B, (HL)
        0x46 => (input, ld_r_iHL(B)),

        _ => unimplemented!(),
    };
    IResult::Done(input, result)
}
