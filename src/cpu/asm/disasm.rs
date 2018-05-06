//! Gameboy dissasembly tools.

use super::Op::{self, *};
use super::Reg8::*;
use super::Reg16::*;
use cpu::MemoryMap;

#[derive(Debug)]
pub enum ParseError {
    UnknownOpcode(Vec<u8>),
}

/// Attempts to disassemble the instruction at the current program counter. Will return the
/// resulting operation, as well as the new value of the PC register (without accounting for
/// possible jumps).
pub fn parse_op<M: MemoryMap>(m: &M, pc: u16) -> Result<(Op, u16), ParseError> {
    let opcode = m.read_u8(pc);

    Ok(match opcode {
        // 8-bit load commands
        // ld B, n
        0x06 => (ld_r_n(B, m.read_u8(pc+1)), pc+2),
        // ld C, n
        0x0E => (ld_r_n(C, m.read_u8(pc+1)), pc+2),
        // ld D, n
        0x16 => (ld_r_n(D, m.read_u8(pc+1)), pc+2),
        // ld E, n
        0x1E => (ld_r_n(E, m.read_u8(pc+1)), pc+2),
        // ld H, n
        0x26 => (ld_r_n(H, m.read_u8(pc+1)), pc+2),
        // ld L, n
        0x2E => (ld_r_n(L, m.read_u8(pc+1)), pc+2),

        // ld A, A
        0x7F => (ld_r_r(A, A), pc+1),
        // ld A, B
        0x78 => (ld_r_r(A, B), pc+1),
        // ld A, C
        0x79 => (ld_r_r(A, C), pc+1),
        // ld A, D
        0x7A => (ld_r_r(A, D), pc+1),
        // ld A, E
        0x7B => (ld_r_r(A, E), pc+1),
        // ld A, H
        0x7C => (ld_r_r(A, H), pc+1),
        // ld A, L
        0x7D => (ld_r_r(A, L), pc+1),
        // ld A, (HL)
        0x7E => (ld_r_iHL(A), pc+1),
        
        // ld B, A
        0x47 => (ld_r_r(B, A), pc+1),
        // ld B, B
        0x40 => (ld_r_r(B, B), pc+1),
        // ld B, C
        0x41 => (ld_r_r(B, C), pc+1),
        // ld B, D
        0x42 => (ld_r_r(B, D), pc+1),
        // ld B, E
        0x43 => (ld_r_r(B, E), pc+1),
        // ld B, H
        0x44 => (ld_r_r(B, H), pc+1),
        // ld B, L
        0x45 => (ld_r_r(B, L), pc+1),
        // ld B, (HL)
        0x46 => (ld_r_iHL(B), pc+1),
        
        // ld C, B
        0x48 => (ld_r_r(C, B), pc+1),
        // ld C, C
        0x49 => (ld_r_r(C, C), pc+1),
        // ld C, D
        0x4A => (ld_r_r(C, D), pc+1),
        // ld C, E
        0x4B => (ld_r_r(C, E), pc+1),
        // ld C, H
        0x4C => (ld_r_r(C, H), pc+1),
        // ld C, L
        0x4D => (ld_r_r(C, L), pc+1),
        // ld C, (HL)
        0x4E => (ld_r_iHL(C), pc+1),

        // ld D, B
        0x50 => (ld_r_r(D, B), pc+1),
        // ld D, C
        0x51 => (ld_r_r(D, C), pc+1),
        // ld D, D
        0x52 => (ld_r_r(D, D), pc+1),
        // ld D, E
        0x53 => (ld_r_r(D, E), pc+1),
        // ld D, H
        0x54 => (ld_r_r(D, H), pc+1),
        // ld D, L
        0x55 => (ld_r_r(D, L), pc+1),
        // ld D, (HL)
        0x56 => (ld_r_iHL(D), pc+1),

        // ld E, B
        0x58 => (ld_r_r(E, B), pc+1),
        // ld E, C
        0x59 => (ld_r_r(E, C), pc+1),
        // ld E, D
        0x5a => (ld_r_r(E, D), pc+1),
        // ld E, E
        0x5b => (ld_r_r(E, E), pc+1),
        // ld E, H
        0x5c => (ld_r_r(E, H), pc+1),
        // ld E, L
        0x5d => (ld_r_r(E, L), pc+1),
        // ld E, (HL)
        0x5e => (ld_r_iHL(E), pc+1),

        // ld H, B
        0x60 => (ld_r_r(H, B), pc+1),
        // ld H, C
        0x61 => (ld_r_r(H, C), pc+1),
        // ld H, D
        0x62 => (ld_r_r(H, D), pc+1),
        // ld H, E
        0x63 => (ld_r_r(H, E), pc+1),
        // ld H, H
        0x64 => (ld_r_r(H, H), pc+1),
        // ld H, L
        0x65 => (ld_r_r(H, L), pc+1),
        // ld H, (HL)
        0x66 => (ld_r_iHL(H), pc+1),

        // ld L, B
        0x68 => (ld_r_r(L, B), pc+1),
        // ld L, C
        0x69 => (ld_r_r(L, C), pc+1),
        // ld L, D
        0x6a => (ld_r_r(L, D), pc+1),
        // ld L, E
        0x6b => (ld_r_r(L, E), pc+1),
        // ld L, H
        0x6c => (ld_r_r(L, H), pc+1),
        // ld L, L
        0x6d => (ld_r_r(L, L), pc+1),
        // ld L, (HL)
        0x6e => (ld_r_iHL(L), pc+1),

        // ld (HL), B
        0x70 => (ld_iHL_r(B), pc+1),
        // ld (HL), C
        0x71 => (ld_iHL_r(C), pc+1),
        // ld (HL), D
        0x72 => (ld_iHL_r(D), pc+1),
        // ld (HL), E
        0x73 => (ld_iHL_r(E), pc+1),
        // ld (HL), H
        0x74 => (ld_iHL_r(H), pc+1),
        // ld (HL), L
        0x75 => (ld_iHL_r(L), pc+1),
        // ld (HL), n
        0x36 => (ld_iHL_n(m.read_u8(pc+1)), pc+2),

        // ld A, (BC)
        0x0a => (ld_A_iBC, pc+1),
        // ld A, (DE)
        0x1a => (ld_A_iDE, pc+1),
        // ld A, (nn)
        0xfa => (ld_A_inn(m.read_u16(pc+1)), pc+3),
        // ld A, n
        0x3e => (ld_r_n(A, m.read_u8(pc+1)), pc+2),

        // ld (BC), A
        0x02 => (ld_iBC_A, pc+1),
        // ld (DE), A
        0x12 => (ld_iDE_A, pc+1),
        // ld (nn), A
        0xea => (ld_inn_A(m.read_u16(pc+1)), pc+3),
        
        // ld A, (C)
        0xf2 => (ld_A_ioC, pc+1),
        // ld (C), A
        0xe2 => (ld_ioC_A, pc+1),
        // ldh A, (n)
        0xf0 => (ld_A_ion(m.read_u8(pc+1)), pc+2),
        // ldh (n), A
        0xe0 => (ld_ion_A(m.read_u8(pc+1)), pc+2),

        // ldd A, (HL)
        0x3a => (ldd_A_iHL, pc+1),
        // ldd (HL), A
        0x32 => (ldd_iHL_A, pc+1),
        // ldi A, (HL)
        0x2a => (ldi_A_iHL, pc+1),
        // ldi (HL), A
        0x22 => (ldi_iHL_A, pc+1),

        // ld BC, nn
        0x01 => (ld_rr_nn(BC, m.read_u16(pc+1)), pc+3),
        // ld DE, nn
        0x11 => (ld_rr_nn(DE, m.read_u16(pc+1)), pc+3),
        // ld HL, nn
        0x21 => (ld_rr_nn(HL, m.read_u16(pc+1)), pc+3),
        // ld SP, nn
        0x31 => (ld_rr_nn(SP, m.read_u16(pc+1)), pc+3),
        // ld SP, HL
        0xf9 => (ld_SP_HL, pc+1),
        // ldhl SP, n
        0xf8 => (ldhl_SP_n(m.read_i8(pc+1)), pc+3),
        // ld (nn), SP
        0x08 => (ld_inn_SP(m.read_u16(pc+1)), pc+3),

        // push AF
        0xf5 => (push(AF), pc+1),
        // push BC
        0xc5 => (push(BC), pc+1),
        // push DE
        0xd5 => (push(DE), pc+1),
        // push HL
        0xe5 => (push(HL), pc+1),

        // pop AF
        0xf1 => (pop(AF), pc+1),
        // pop BC
        0xc1 => (pop(BC), pc+1),
        // pop DE
        0xd1 => (pop(DE), pc+1),
        // pop HL
        0xe1 => (pop(HL), pc+1),

        // add A, A
        0x87 => (add_A_r(A), pc+1),
        // add A, B
        0x80 => (add_A_r(B), pc+1),
        // add A, C
        0x81 => (add_A_r(C), pc+1),
        // add A, D
        0x82 => (add_A_r(D), pc+1),
        // add A, E
        0x83 => (add_A_r(E), pc+1),
        // add A, H
        0x84 => (add_A_r(H), pc+1),
        // add A, L
        0x85 => (add_A_r(L), pc+1),
        // add A, (HL)
        0x86 => (add_A_iHL, pc+1),
        // add A, n
        0xc6 => (add_A_n(m.read_u8(pc+1)), pc+2),

        // adc A, A
        0x8f => (adc_A_r(A), pc+1),
        // adc A, B
        0x88 => (adc_A_r(B), pc+1),
        // adc A, C
        0x89 => (adc_A_r(C), pc+1),
        // adc A, D
        0x8a => (adc_A_r(D), pc+1),
        // adc A, E
        0x8b => (adc_A_r(E), pc+1),
        // adc A, H
        0x8c => (adc_A_r(H), pc+1),
        // adc A, L
        0x8d => (adc_A_r(L), pc+1),
        // adc A, (HL)
        0x8e => (adc_A_iHL, pc+1),
        // adc A, n
        0xce => (adc_A_n(m.read_u8(pc+1)), pc+2),

        // sub A
        0x97 => (sub_r(A), pc+1),
        // sub B
        0x90 => (sub_r(B), pc+1),
        // sub C
        0x91 => (sub_r(C), pc+1),
        // sub D
        0x92 => (sub_r(D), pc+1),
        // sub E
        0x93 => (sub_r(E), pc+1),
        // sub H
        0x94 => (sub_r(H), pc+1),
        // sub L
        0x95 => (sub_r(L), pc+1),
        // sub (HL)
        0x96 => (sub_iHL, pc+1),
        // sub n
        0xd6 => (sub_n(m.read_u8(pc+1)), pc+2),

        // sbc A, A
        0x9f => (sbc_A_r(A), pc+1),
        // sbc A, B
        0x98 => (sbc_A_r(B), pc+1),
        // sbc A, C
        0x99 => (sbc_A_r(C), pc+1),
        // sbc A, D
        0x9a => (sbc_A_r(D), pc+1),
        // sbc A, E
        0x9b => (sbc_A_r(E), pc+1),
        // sbc A, H
        0x9c => (sbc_A_r(H), pc+1),
        // sbc A, L
        0x9d => (sbc_A_r(L), pc+1),
        // sbc A, (HL)
        0x9e => (sbc_A_iHL, pc+1),
        // sbc A, n
        0xde => (sbc_A_n(m.read_u8(pc+1)), pc+2),

        // and A
        0xa7 => (and_r(A), pc+1),
        // and B
        0xa0 => (and_r(B), pc+1),
        // and C
        0xa1 => (and_r(C), pc+1),
        // and D
        0xa2 => (and_r(D), pc+1),
        // and E
        0xa3 => (and_r(E), pc+1),
        // and H
        0xa4 => (and_r(H), pc+1),
        // and L
        0xa5 => (and_r(L), pc+1),
        // and (HL)
        0xa6 => (and_iHL, pc+1),
        // and n
        0xe6 => (and_n(m.read_u8(pc+1)), pc+2),

        // or A
        0xb7 => (or_r(A), pc+1),
        // or B
        0xb0 => (or_r(B), pc+1),
        // or C
        0xb1 => (or_r(C), pc+1),
        // or D
        0xb2 => (or_r(D), pc+1),
        // or E
        0xb3 => (or_r(E), pc+1),
        // or H
        0xb4 => (or_r(H), pc+1),
        // or L
        0xb5 => (or_r(L), pc+1),
        // or (HL)
        0xb6 => (or_iHL, pc+1),
        // or n
        0xf6 => (or_n(m.read_u8(pc+1)), pc+2),

        // xor A
        0xaf => (xor_r(A), pc+1),
        // xor B
        0xa8 => (xor_r(B), pc+1),
        // xor C
        0xa9 => (xor_r(C), pc+1),
        // xor D
        0xaa => (xor_r(D), pc+1),
        // xor E
        0xab => (xor_r(E), pc+1),
        // xor H
        0xac => (xor_r(H), pc+1),
        // xor L
        0xad => (xor_r(L), pc+1),
        // xor (HL)
        0xae => (xor_iHL, pc+1),
        // xor n
        0xee => (xor_n(m.read_u8(pc+1)), pc+2),

        // cp A
        0xbf => (cp_r(A), pc+1),
        // cp B
        0xb8 => (cp_r(B), pc+1),
        // cp C
        0xb9 => (cp_r(C), pc+1),
        // cp D
        0xba => (cp_r(D), pc+1),
        // cp E
        0xbb => (cp_r(E), pc+1),
        // cp H
        0xbc => (cp_r(H), pc+1),
        // cp L
        0xbd => (cp_r(L), pc+1),
        // cp (HL)
        0xbe => (cp_iHL, pc+1),
        // cp n
        0xfe => (cp_n(m.read_u8(pc+1)), pc+2),

        // inc A
        0x3c => (inc_r(A), pc+1),
        // inc B
        0x04 => (inc_r(B), pc+1),
        // inc C
        0x0c => (inc_r(C), pc+1),
        // inc D
        0x14 => (inc_r(D), pc+1),
        // inc E
        0x1c => (inc_r(E), pc+1),
        // inc H
        0x24 => (inc_r(H), pc+1),
        // inc L
        0x2c => (inc_r(L), pc+1),
        // inc (HL)
        0x34 => (inc_iHL, pc+1),

        // dec A
        0x3d => (dec_r(A), pc+1),
        // dec B
        0x05 => (dec_r(B), pc+1),
        // dec C
        0x0d => (dec_r(C), pc+1),
        // dec D
        0x15 => (dec_r(D), pc+1),
        // dec E
        0x1d => (dec_r(E), pc+1),
        // dec H
        0x25 => (dec_r(H), pc+1),
        // dec L
        0x2d => (dec_r(L), pc+1),
        // dec (HL)
        0x35 => (dec_iHL, pc+1),

        // add HL, BC
        0x09 => (add_HL_rr(BC), pc+1),
        // add HL, DE
        0x19 => (add_HL_rr(DE), pc+1),
        // add HL, HL
        0x29 => (add_HL_rr(HL), pc+1),
        // add HL, SP
        0x39 => (add_HL_rr(SP), pc+1),
        // add SP, n
        0xe8 => (add_SP_n(m.read_i8(pc+1)), pc+2),
        
        // inc BC
        0x03 => (inc_rr(BC), pc+1),
        // inc DE
        0x13 => (inc_rr(DE), pc+1),
        // inc HL
        0x23 => (inc_rr(HL), pc+1),
        // inc SP
        0x33 => (inc_rr(SP), pc+1),

        // dec BC
        0x0b => (dec_rr(BC), pc+1),
        // dec DE
        0x1b => (dec_rr(DE), pc+1),
        // dec HL
        0x2b => (dec_rr(HL), pc+1),
        // dec SP
        0x3b => (dec_rr(SP), pc+1),

        // daa
        0x27 => (daa, pc+1),
        // cpl
        0x2f => (cpl, pc+1),
        // ccf
        0x3f => (ccf, pc+1),
        // scf
        0x37 => (scf, pc+1),
        // nop
        0x00 => (nop, pc+1),
        // halt
        0x76 => (halt, pc+1),
        // stop
        0x10 => {
            let op = m.read_u8(pc+1);
            if op == 0x00 {
                (stop, pc+2)
            } else {
                return Err(ParseError::UnknownOpcode(vec![0x10, op]))
            }
        },
        // di
        0xf3 => (di, pc+1),
        // ei
        0xfb => (ei, pc+1),

        // rlca
        0x07 => (rlca, pc+1),
        // rla
        0x17 => (rla, pc+1),
        // rrca
        0x0f => (rrca, pc+1),
        // rra
        0x1f => (rra, pc+1),

        // jp nn
        0xc3 => (jp(m.read_u16(pc+1)), pc+3),
        // jp NZ, nn
        0xc2 => (jp_NZ(m.read_u16(pc+1)), pc+3),
        // jp Z, nn
        0xca => (jp_Z(m.read_u16(pc+1)), pc+3),
        // jp NC, nn
        0xd2 => (jp_NC(m.read_u16(pc+1)), pc+3),
        // jp C, nn
        0xda => (jp_C(m.read_u16(pc+1)), pc+3),
        // jp (HL)
        0xe9 => (jp_iHL, pc+1),
        
        // jr n
        0x18 => (jr(m.read_i8(pc+1)), pc+2),
        // jr NZ, n
        0x20 => (jr_NZ(m.read_i8(pc+1)), pc+2),
        // jr Z, n
        0x28 => (jr_Z(m.read_i8(pc+1)), pc+2),
        // jr NC, n
        0x30 => (jr_NC(m.read_i8(pc+1)), pc+2),
        // jr C, n
        0x38 => (jr_C(m.read_i8(pc+1)), pc+2),

        // call nn
        0xcd => (call(m.read_u16(pc+1)), pc+3),
        // call NZ, nn
        0xc4 => (call_NZ(m.read_u16(pc+1)), pc+3),
        // call Z, nn
        0xcc => (call_Z(m.read_u16(pc+1)), pc+3),
        // call NC, nn
        0xd4 => (call_NC(m.read_u16(pc+1)), pc+3),
        // call C, nn
        0xdc => (call_C(m.read_u16(pc+1)), pc+3),

        // rst 00h
        0xc7 => (rst_00, pc+1),
        // rst 08h
        0xcf => (rst_08, pc+1),
        // rst 10h
        0xd7 => (rst_10, pc+1),
        // rst 18h
        0xdf => (rst_18, pc+1),
        // rst 20h
        0xe7 => (rst_20, pc+1),
        // rst 28h
        0xef => (rst_28, pc+1),
        // rst 30h
        0xf7 => (rst_30, pc+1),
        // rst 38h
        0xff => (rst_38, pc+1),

        // ret
        0xc9 => (ret, pc+1),
        // ret NZ
        0xc0 => (ret_NZ, pc+1),
        // ret Z
        0xc8 => (ret_Z, pc+1),
        // ret NC 
        0xd0 => (ret_NC, pc+1),
        // ret C
        0xd8 => (ret_C, pc+1),
        // reti
        0xd9 => (reti, pc+1),
        
        0xcb => return parse_cb(m, pc+1),

        opcode => return Err(ParseError::UnknownOpcode(vec![opcode])),
    })
}

fn parse_cb<M: MemoryMap>(_m: &M, _pc: u16) -> Result<(Op, u16), ParseError> {
    unimplemented!()
}

/*
fn parse_cb<M: MemoryMap>(m: &M, pc: u16) -> Result<(Op, u16), ParseError> {
    let op = m[pc];

            Ok(match op {
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

                op => return Err(ParseError::UnknownOpcode(vec![0xce, op])),
            })
}
*/
