use super::*;
use self::Op::*;
use self::op::Reg8::*;
use self::op::Reg16::*;
use cpu::MemoryMap;

const DATA: &[(&'static [u8], Op, &'static str)] = &[
    (b"\x06\x65", ld_r_n(B, 0x65), "ld B, $65"),
    (b"\x0e\x12", ld_r_n(C, 0x12), "ld C, $12"),
    (b"\x16\x00", ld_r_n(D, 0x00), "ld D, $00"),
    (b"\x1e\x08", ld_r_n(E, 0x08), "ld E, $08"),
    (b"\x26\xa9", ld_r_n(H, 0xa9), "ld H, $a9"),
    (b"\x2e\x99", ld_r_n(L, 0x99), "ld L, $99"),
    (b"\x7f", ld_r_r(A, A), "ld A, A"),
    (b"\x78", ld_r_r(A, B), "ld A, B"),
    (b"\x79", ld_r_r(A, C), "ld A, C"),
    (b"\x7a", ld_r_r(A, D), "ld A, D"),
    (b"\x7b", ld_r_r(A, E), "ld A, E"),
    (b"\x7c", ld_r_r(A, H), "ld A, H"),
    (b"\x7d", ld_r_r(A, L), "ld A, L"),
    (b"\x7e", ld_r_iHL(A), "ld A, (HL)"),
    (b"\x40", ld_r_r(B, B), "ld B, B"),
    (b"\x41", ld_r_r(B, C), "ld B, C"),
    (b"\x42", ld_r_r(B, D), "ld B, D"),
    (b"\x43", ld_r_r(B, E), "ld B, E"),
    (b"\x44", ld_r_r(B, H), "ld B, H"),
    (b"\x45", ld_r_r(B, L), "ld B, L"),
    (b"\x46", ld_r_iHL(B), "ld B, (HL)"),
    (b"\x48", ld_r_r(C, B), "ld C, B"),
    (b"\x49", ld_r_r(C, C), "ld C, C"),
    (b"\x4a", ld_r_r(C, D), "ld C, D"),
    (b"\x4b", ld_r_r(C, E), "ld C, E"),
    (b"\x4c", ld_r_r(C, H), "ld C, H"),
    (b"\x4d", ld_r_r(C, L), "ld C, L"),
    (b"\x4e", ld_r_iHL(C), "ld C, (HL)"),
    (b"\x50", ld_r_r(D, B), "ld D, B"),
    (b"\x51", ld_r_r(D, C), "ld D, C"),
    (b"\x52", ld_r_r(D, D), "ld D, D"),
    (b"\x53", ld_r_r(D, E), "ld D, E"),
    (b"\x54", ld_r_r(D, H), "ld D, H"),
    (b"\x55", ld_r_r(D, L), "ld D, L"),
    (b"\x56", ld_r_iHL(D), "ld D, (HL)"),
    (b"\x58", ld_r_r(E, B), "ld E, B"),
    (b"\x59", ld_r_r(E, C), "ld E, C"),
    (b"\x5a", ld_r_r(E, D), "ld E, D"),
    (b"\x5b", ld_r_r(E, E), "ld E, E"),
    (b"\x5c", ld_r_r(E, H), "ld E, H"),
    (b"\x5d", ld_r_r(E, L), "ld E, L"),
    (b"\x5e", ld_r_iHL(E), "ld E, (HL)"),
    (b"\x60", ld_r_r(H, B), "ld H, B"),
    (b"\x61", ld_r_r(H, C), "ld H, C"),
    (b"\x62", ld_r_r(H, D), "ld H, D"),
    (b"\x63", ld_r_r(H, E), "ld H, E"),
    (b"\x64", ld_r_r(H, H), "ld H, H"),
    (b"\x65", ld_r_r(H, L), "ld H, L"),
    (b"\x66", ld_r_iHL(H), "ld H, (HL)"),
    (b"\x68", ld_r_r(L, B), "ld L, B"),
    (b"\x69", ld_r_r(L, C), "ld L, C"),
    (b"\x6a", ld_r_r(L, D), "ld L, D"),
    (b"\x6b", ld_r_r(L, E), "ld L, E"),
    (b"\x6c", ld_r_r(L, H), "ld L, H"),
    (b"\x6d", ld_r_r(L, L), "ld L, L"),
    (b"\x6e", ld_r_iHL(L), "ld L, (HL)"),
    (b"\x70", ld_iHL_r(B), "ld (HL), B"),
    (b"\x71", ld_iHL_r(C), "ld (HL), C"),
    (b"\x72", ld_iHL_r(D), "ld (HL), D"),
    (b"\x73", ld_iHL_r(E), "ld (HL), E"),
    (b"\x74", ld_iHL_r(H), "ld (HL), H"),
    (b"\x75", ld_iHL_r(L), "ld (HL), L"),
    (b"\x36\x04", ld_iHL_n(0x04), "ld (HL), $04"),

    (b"\x31\x0d\x00", ld_rr_nn(SP, 0x000d), "ld SP, $000d"),
];

struct Mapper<'a>(&'a mut [u8]);

impl<'a> MemoryMap for Mapper<'a> {
    fn read_u8(&self, offset: u16) -> u8 {
        self.0[offset as usize]
    }

    fn write_u8(&mut self, offset: u16, value: u8) {
        self.0[offset as usize] = value
    }
}

#[test]
fn disassembly() {
    for (data, expected, formatted) in DATA.iter() {
        let mut data = data.to_vec();
        let (result, len) = parse_op(&Mapper(&mut data), 0).unwrap();
        assert_eq!(result, *expected);
        assert_eq!(result.to_string(), *formatted);
        assert_eq!(len as usize, data.len());
    }
}
