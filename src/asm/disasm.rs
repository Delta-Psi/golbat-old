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
        
        // ld C, B
        0x48 => (input, ld_r_r(C, B)),
        // ld C, C
        0x49 => (input, ld_r_r(C, C)),
        // ld C, D
        0x4A => (input, ld_r_r(C, D)),
        // ld C, E
        0x4B => (input, ld_r_r(C, E)),
        // ld C, H
        0x4C => (input, ld_r_r(C, H)),
        // ld C, L
        0x4D => (input, ld_r_r(C, L)),
        // ld C, (HL)
        0x4E => (input, ld_r_iHL(C)),

        // ld D, B
        0x50 => (input, ld_r_r(D, B)),
        // ld D, C
        0x51 => (input, ld_r_r(D, C)),
        // ld D, D
        0x52 => (input, ld_r_r(D, D)),
        // ld D, E
        0x53 => (input, ld_r_r(D, E)),
        // ld D, H
        0x54 => (input, ld_r_r(D, H)),
        // ld D, L
        0x55 => (input, ld_r_r(D, L)),
        // ld D, (HL)
        0x56 => (input, ld_r_iHL(D)),

        // ld E, B
        0x58 => (input, ld_r_r(E, B)),
        // ld E, C
        0x59 => (input, ld_r_r(E, C)),
        // ld E, D
        0x5a => (input, ld_r_r(E, D)),
        // ld E, E
        0x5b => (input, ld_r_r(E, E)),
        // ld E, H
        0x5c => (input, ld_r_r(E, H)),
        // ld E, L
        0x5d => (input, ld_r_r(E, L)),
        // ld E, (HL)
        0x5e => (input, ld_r_iHL(E)),

        // ld H, B
        0x60 => (input, ld_r_r(H, B)),
        // ld H, C
        0x61 => (input, ld_r_r(H, C)),
        // ld H, D
        0x62 => (input, ld_r_r(H, D)),
        // ld H, E
        0x63 => (input, ld_r_r(H, E)),
        // ld H, H
        0x64 => (input, ld_r_r(H, H)),
        // ld H, L
        0x65 => (input, ld_r_r(H, L)),
        // ld H, (HL)
        0x66 => (input, ld_r_iHL(H)),

        // ld L, B
        0x68 => (input, ld_r_r(L, B)),
        // ld L, C
        0x69 => (input, ld_r_r(L, C)),
        // ld L, D
        0x6a => (input, ld_r_r(L, D)),
        // ld L, E
        0x6b => (input, ld_r_r(L, E)),
        // ld L, H
        0x6c => (input, ld_r_r(L, H)),
        // ld L, L
        0x6d => (input, ld_r_r(L, L)),
        // ld L, (HL)
        0x6e => (input, ld_r_iHL(L)),

        // ld (HL), B
        0x70 => (input, ld_iHL_r(B)),
        // ld (HL), C
        0x71 => (input, ld_iHL_r(C)),
        // ld (HL), D
        0x72 => (input, ld_iHL_r(D)),
        // ld (HL), E
        0x73 => (input, ld_iHL_r(E)),
        // ld (HL), H
        0x74 => (input, ld_iHL_r(H)),
        // ld (HL), L
        0x75 => (input, ld_iHL_r(L)),
        // ld (HL), n
        0x36 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_iHL_n(value))
        },

        // ld A, (BC)
        0x0a => (input, ld_A_iBC),
        // ld A, (DE)
        0x1a => (input, ld_A_iDE),
        // ld A, (nn)
        0xfa => {
            let (input, value) = try_parse!(input, le_u16);
            (input, ld_A_inn(value))
        },
        // ld A, n
        0x3e => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_r_n(A, value))
        },

        // ld (BC), A
        0x02 => (input, ld_iBC_A),
        // ld (DE), A
        0x12 => (input, ld_iDE_A),
        // ld (nn), A
        0xea => {
            let (input, value) = try_parse!(input, le_u16);
            (input, ld_inn_A(value))
        },
        
        // ld A, (C)
        0xf2 => (input, ld_A_ioC),
        // ld (C), A
        0xe2 => (input, ld_ioC_A),
        // ldh A, (n)
        0xf0 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_A_ion(value))
        },
        // ldh (n), A
        0xe0 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, ld_ion_A(value))
        },

        // ldd A, (HL)
        0x3a => (input, ldd_A_iHL),
        // ldd (HL), A
        0x32 => (input, ldd_iHL_A),
        // ldi A, (HL)
        0x2a => (input, ldi_A_iHL),
        // ldi (HL), A
        0x22 => (input, ldi_iHL_A),

        // ld BC, nn
        0x01 => {
            let (input, value) = try_parse!(input, le_u16);
            (input, ld_rr_nn(BC, value))
        },
        // ld DE, nn
        0x11 => {
            let (input, value) = try_parse!(input, le_u16);
            (input, ld_rr_nn(DE, value))
        },
        // ld HL, nn
        0x21 => {
            let (input, value) = try_parse!(input, le_u16);
            (input, ld_rr_nn(HL, value))
        },
        // ld SP, nn
        0x31 => {
            let (input, value) = try_parse!(input, le_u16);
            (input, ld_rr_nn(SP, value))
        },
        // ld SP, HL
        0xf9 => (input, ld_SP_HL),
        // ldhl SP, n
        0xf8 => {
            let (input, value) = try_parse!(input, le_i8);
            (input, ldhl_SP_n(value))
        },
        // ld (nn), SP
        0x08 => {
            let (input, value) = try_parse!(input, le_u16);
            (input, ld_inn_SP(value))
        },

        // push AF
        0xf5 => (input, push(AF)),
        // push BC
        0xc5 => (input, push(BC)),
        // push DE
        0xd5 => (input, push(DE)),
        // push HL
        0xe5 => (input, push(HL)),

        // pop AF
        0xf1 => (input, pop(AF)),
        // pop BC
        0xc1 => (input, pop(BC)),
        // pop DE
        0xd1 => (input, pop(DE)),
        // pop HL
        0xe1 => (input, pop(HL)),

        // add A, A
        0x87 => (input, add_A_r(A)),
        // add A, B
        0x80 => (input, add_A_r(B)),
        // add A, C
        0x81 => (input, add_A_r(C)),
        // add A, D
        0x82 => (input, add_A_r(D)),
        // add A, E
        0x83 => (input, add_A_r(E)),
        // add A, H
        0x84 => (input, add_A_r(H)),
        // add A, L
        0x85 => (input, add_A_r(L)),
        // add A, (HL)
        0x86 => (input, add_A_iHL),
        // add A, n
        0xc6 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, add_A_n(value))
        },

        // adc A, A
        0x8f => (input, adc_A_r(A)),
        // adc A, B
        0x88 => (input, adc_A_r(B)),
        // adc A, C
        0x89 => (input, adc_A_r(C)),
        // adc A, D
        0x8a => (input, adc_A_r(D)),
        // adc A, E
        0x8b => (input, adc_A_r(E)),
        // adc A, H
        0x8c => (input, adc_A_r(H)),
        // adc A, L
        0x8d => (input, adc_A_r(L)),
        // adc A, (HL)
        0x8e => (input, adc_A_iHL),
        // adc A, n
        0xce => {
            let (input, value) = try_parse!(input, le_u8);
            (input, adc_A_n(value))
        },

        // sub A
        0x97 => (input, sub_r(A)),
        // sub B
        0x90 => (input, sub_r(B)),
        // sub C
        0x91 => (input, sub_r(C)),
        // sub D
        0x92 => (input, sub_r(D)),
        // sub E
        0x93 => (input, sub_r(E)),
        // sub H
        0x94 => (input, sub_r(H)),
        // sub L
        0x95 => (input, sub_r(L)),
        // sub (HL)
        0x96 => (input, sub_iHL),
        // sub n
        0xd6 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, sub_n(value))
        },

        // sbc A, A
        0x9f => (input, sbc_A_r(A)),
        // sbc A, B
        0x98 => (input, sbc_A_r(B)),
        // sbc A, C
        0x99 => (input, sbc_A_r(C)),
        // sbc A, D
        0x9a => (input, sbc_A_r(D)),
        // sbc A, E
        0x9b => (input, sbc_A_r(E)),
        // sbc A, H
        0x9c => (input, sbc_A_r(H)),
        // sbc A, L
        0x9d => (input, sbc_A_r(L)),
        // sbc A, (HL)
        0x9e => (input, sbc_A_iHL),
        // sbc A, n
        0xde => {
            let (input, value) = try_parse!(input, le_u8);
            (input, sbc_A_n(value))
        },

        _ => unimplemented!(),
    };
    IResult::Done(input, result)
}
