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

        // and A
        0xa7 => (input, and_r(A)),
        // and B
        0xa0 => (input, and_r(B)),
        // and C
        0xa1 => (input, and_r(C)),
        // and D
        0xa2 => (input, and_r(D)),
        // and E
        0xa3 => (input, and_r(E)),
        // and H
        0xa4 => (input, and_r(H)),
        // and L
        0xa5 => (input, and_r(L)),
        // and (HL)
        0xa6 => (input, and_iHL),
        // and n
        0xe6 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, and_n(value))
        },

        // or A
        0xb7 => (input, or_r(A)),
        // or B
        0xb0 => (input, or_r(B)),
        // or C
        0xb1 => (input, or_r(C)),
        // or D
        0xb2 => (input, or_r(D)),
        // or E
        0xb3 => (input, or_r(E)),
        // or H
        0xb4 => (input, or_r(H)),
        // or L
        0xb5 => (input, or_r(L)),
        // or (HL)
        0xb6 => (input, or_iHL),
        // or n
        0xf6 => {
            let (input, value) = try_parse!(input, le_u8);
            (input, or_n(value))
        },

        // xor A
        0xaf => (input, xor_r(A)),
        // xor B
        0xa8 => (input, xor_r(B)),
        // xor C
        0xa9 => (input, xor_r(C)),
        // xor D
        0xaa => (input, xor_r(D)),
        // xor E
        0xab => (input, xor_r(E)),
        // xor H
        0xac => (input, xor_r(H)),
        // xor L
        0xad => (input, xor_r(L)),
        // xor (HL)
        0xae => (input, xor_iHL),
        // xor n
        0xee => {
            let (input, value) = try_parse!(input, le_u8);
            (input, xor_n(value))
        },

        // cp A
        0xbf => (input, cp_r(A)),
        // cp B
        0xb8 => (input, cp_r(B)),
        // cp C
        0xb9 => (input, cp_r(C)),
        // cp D
        0xba => (input, cp_r(D)),
        // cp E
        0xbb => (input, cp_r(E)),
        // cp H
        0xbc => (input, cp_r(H)),
        // cp L
        0xbd => (input, cp_r(L)),
        // cp (HL)
        0xbe => (input, cp_iHL),
        // cp n
        0xfe => {
            let (input, value) = try_parse!(input, le_u8);
            (input, cp_n(value))
        },

        // inc A
        0x3c => (input, inc_r(A)),
        // inc B
        0x04 => (input, inc_r(B)),
        // inc C
        0x0c => (input, inc_r(C)),
        // inc D
        0x14 => (input, inc_r(D)),
        // inc E
        0x1c => (input, inc_r(E)),
        // inc H
        0x24 => (input, inc_r(H)),
        // inc L
        0x2c => (input, inc_r(L)),
        // inc (HL)
        0x34 => (input, inc_iHL),

        // dec A
        0x3d => (input, dec_r(A)),
        // dec B
        0x05 => (input, dec_r(B)),
        // dec C
        0x0d => (input, dec_r(C)),
        // dec D
        0x15 => (input, dec_r(D)),
        // dec E
        0x1d => (input, dec_r(E)),
        // dec H
        0x25 => (input, dec_r(H)),
        // dec L
        0x2d => (input, dec_r(L)),
        // dec (HL)
        0x35 => (input, dec_iHL),

        // add HL, BC
        0x09 => (input, add_HL_rr(BC)),
        // add HL, DE
        0x19 => (input, add_HL_rr(DE)),
        // add HL, HL
        0x29 => (input, add_HL_rr(HL)),
        // add HL, SP
        0x39 => (input, add_HL_rr(SP)),
        // add SP, n
        0xe8 => {
            let (input, value) = try_parse!(input, le_i8);
            (input, add_SP_n(value))
        },
        
        // inc BC
        0x03 => (input, inc_rr(BC)),
        // inc DE
        0x13 => (input, inc_rr(DE)),
        // inc HL
        0x23 => (input, inc_rr(HL)),
        // inc SP
        0x33 => (input, inc_rr(SP)),

        // dec BC
        0x0b => (input, dec_rr(BC)),
        // dec DE
        0x1b => (input, dec_rr(DE)),
        // dec HL
        0x2b => (input, dec_rr(HL)),
        // dec SP
        0x3b => (input, dec_rr(SP)),

        // daa
        0x27 => (input, daa),
        // cpl
        0x2f => (input, cpl),
        // ccf
        0x3f => (input, ccf),
        // scf
        0x37 => (input, scf),
        // nop
        0x00 => (input, nop),
        // halt
        0x76 => (input, halt),
        // stop
        0x10 => {
            let (input, op) = try_parse!(input, le_u8);
            if op == 0x00 {
                (input, stop)
            } else {
                return IResult::Error(ErrorKind::Custom(1))
            }
        },
        // di
        0xf3 => (input, di),
        // ei
        0xfb => (input, ei),

        // rlca
        0x07 => (input, rlca),
        // rla
        0x17 => (input, rla),
        // rrca
        0x0f => (input, rrca),
        // rra
        0x1f => (input, rra),

        // jp nn
        0xc3 => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, jp(offset))
        },
        // jp NZ, nn
        0xc2 => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, jp_NZ(offset))
        },
        // jp Z, nn
        0xca => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, jp_Z(offset))
        },
        // jp NC, nn
        0xd2 => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, jp_NC(offset))
        },
        // jp C, nn
        0xda => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, jp_C(offset))
        },
        // jp (HL)
        0xe9 => (input, jp_iHL),
        
        // jr n
        0x18 => {
            let (input, offset) = try_parse!(input, le_i8);
            (input, jr(offset))
        },
        // jr NZ, n
        0x20 => {
            let (input, offset) = try_parse!(input, le_i8);
            (input, jr_NZ(offset))
        },
        // jr Z, n
        0x28 => {
            let (input, offset) = try_parse!(input, le_i8);
            (input, jr_Z(offset))
        },
        // jr NC, n
        0x30 => {
            let (input, offset) = try_parse!(input, le_i8);
            (input, jr_NC(offset))
        },
        // jr C, n
        0x38 => {
            let (input, offset) = try_parse!(input, le_i8);
            (input, jr_C(offset))
        },

        // call nn
        0xcd => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, call(offset))
        },
        // call NZ, nn
        0xc4 => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, call_NZ(offset))
        },
        // call Z, nn
        0xcc => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, call_Z(offset))
        },
        // call NC, nn
        0xd4 => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, call_NC(offset))
        },
        // call C, nn
        0xdc => {
            let (input, offset) = try_parse!(input, le_u16);
            (input, call_C(offset))
        },

        // rst 00h
        0xc7 => (input, rst_00),
        // rst 08h
        0xcf => (input, rst_08),
        // rst 10h
        0xd7 => (input, rst_10),
        // rst 18h
        0xdf => (input, rst_18),
        // rst 20h
        0xe7 => (input, rst_20),
        // rst 28h
        0xef => (input, rst_28),
        // rst 30h
        0xf7 => (input, rst_30),
        // rst 38h
        0xff => (input, rst_38),

        // ret
        0xc9 => (input, ret),
        // ret NZ
        0xc0 => (input, ret_NZ),
        // ret Z
        0xc8 => (input, ret_Z),
        // ret NC 
        0xd0 => (input, ret_NC),
        // ret C
        0xd8 => (input, ret_C),
        // reti
        0xd9 => (input, reti),
        
        0xcb => try_parse!(input, parse_cb),

        _ => panic!("unknown opcode"),
    };
    IResult::Done(input, result)
}

fn parse_cb(input: &[u8]) -> IResult<&[u8], Op, u32> {
            let (input, op) = try_parse!(input, le_u8);
            let (input, result) = match op {
                // swap A
                0x37 => (input, swap_r(A)),
                // swap B
                0x30 => (input, swap_r(B)),
                // swap C
                0x31 => (input, swap_r(C)),
                // swap D
                0x32 => (input, swap_r(D)),
                // swap E
                0x33 => (input, swap_r(E)),
                // swap H
                0x34 => (input, swap_r(H)),
                // swap L
                0x35 => (input, swap_r(L)),
                // swap (HL)
                0x36 => (input, swap_iHL),

                // rlc A
                0x07 => (input, rlc_r(A)),
                // rlc B
                0x00 => (input, rlc_r(B)),
                // rlc C
                0x01 => (input, rlc_r(C)),
                // rlc D
                0x02 => (input, rlc_r(D)),
                // rlc E
                0x03 => (input, rlc_r(E)),
                // rlc H
                0x04 => (input, rlc_r(H)),
                // rlc L
                0x05 => (input, rlc_r(L)),
                // rlc (HL)
                0x06 => (input, rlc_iHL),

                // rl A
                0x17 => (input, rl_r(A)),
                // rl B
                0x10 => (input, rl_r(B)),
                // rl C
                0x11 => (input, rl_r(C)),
                // rl D
                0x12 => (input, rl_r(D)),
                // rl E
                0x13 => (input, rl_r(E)),
                // rl H
                0x14 => (input, rl_r(H)),
                // rl L
                0x15 => (input, rl_r(L)),
                // rl (HL)
                0x16 => (input, rl_iHL),

                // rrc A
                0x0f => (input, rrc_r(A)),
                // rrc B
                0x08 => (input, rrc_r(B)),
                // rrc C
                0x09 => (input, rrc_r(C)),
                // rrc D
                0x0a => (input, rrc_r(D)),
                // rrc E
                0x0b => (input, rrc_r(E)),
                // rrc H
                0x0c => (input, rrc_r(H)),
                // rrc L
                0x0d => (input, rrc_r(H)),
                // rrc (HL)
                0x0e => (input, rrc_iHL),

                // rr A
                0x1f => (input, rr_r(A)),
                // rr B
                0x18 => (input, rr_r(B)),
                // rr C
                0x19 => (input, rr_r(C)),
                // rr D
                0x1a => (input, rr_r(D)),
                // rr E
                0x1b => (input, rr_r(E)),
                // rr H
                0x1c => (input, rr_r(H)),
                // rr L
                0x1d => (input, rr_r(L)),
                // rr (HI)
                0x1e => (input, rr_iHL),

                // sla A
                0x27 => (input, sla_r(A)),
                // sla B
                0x20 => (input, sla_r(B)),
                // sla C
                0x21 => (input, sla_r(C)),
                // sla D
                0x22 => (input, sla_r(D)),
                // sla E
                0x23 => (input, sla_r(E)),
                // sla H
                0x24 => (input, sla_r(H)),
                // sla L
                0x25 => (input, sla_r(L)),
                // sla (HL)
                0x26 => (input, sla_iHL),

                // sra A
                0x2f => (input, sra_r(A)),
                // sra B
                0x28 => (input, sra_r(B)),
                // sra C
                0x29 => (input, sra_r(C)),
                // sra D
                0x2a => (input, sra_r(D)),
                // sra E
                0x2b => (input, sra_r(E)),
                // sra H
                0x2c => (input, sra_r(H)),
                // sra L
                0x2d => (input, sra_r(L)),
                // sra (HL)
                0x2e => (input, sra_iHL),

                // srl A
                0x3f => (input, srl_r(A)),
                // srl B
                0x38 => (input, srl_r(B)),
                // srl C
                0x39 => (input, srl_r(C)),
                // srl D
                0x3a => (input, srl_r(D)),
                // srl E
                0x3b => (input, srl_r(E)),
                // srl H
                0x3c => (input, srl_r(H)),
                // srl L
                0x3d => (input, srl_r(L)),
                // srl (HL)
                0x3e => (input, srl_iHL),

                // bit b, A
                0x47 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_r(bit, A))
                },
                // bit b, B
                0x40 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_r(bit, B))
                },
                // bit b, C
                0x41 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_r(bit, C))
                },
                // bit b, D
                0x42 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_r(bit, D))
                },
                // bit b, E
                0x43 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_r(bit, E))
                },
                // bit b, H
                0x44 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_r(bit, H))
                },
                // bit b, L
                0x45 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_r(bit, L))
                },
                // bit b, (HL)
                0x46 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, bit_iHL(bit))
                },

                // set b, A
                0xc7 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_r(bit, A))
                },
                // set b, B
                0xc0 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_r(bit, B))
                },
                // set b, C
                0xc1 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_r(bit, C))
                },
                // set b, D
                0xc2 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_r(bit, D))
                },
                // set b, E
                0xc3 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_r(bit, E))
                },
                // set b, H
                0xc4 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_r(bit, H))
                },
                // set b, L
                0xc5 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_r(bit, L))
                },
                // set b, (HL)
                0xc6 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, set_iHL(bit))
                },
                
                // res b, A
                0x87 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_r(bit, A))
                },
                // res b, B
                0x80 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_r(bit, B))
                },
                // res b, C
                0x81 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_r(bit, C))
                },
                // res b, D
                0x82 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_r(bit, D))
                },
                // res b, E
                0x83 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_r(bit, E))
                },
                // res b, H
                0x84 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_r(bit, H))
                },
                // res b, L
                0x85 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_r(bit, L))
                },
                // res b, (HL)
                0x86 => {
                    let (input, bit) = try_parse!(input, le_u8);
                    assert!(bit < 8);
                    (input, res_iHL(bit))
                },

                _ => panic!("unknown CB-opcode"),
                //_ => return IResult::Error(ErrorKind::Custom(1)),
            };
    IResult::Done(input, result)
}
