use super::*;
use self::Op::*;
use self::op::Reg8::*;
use self::op::Reg16::*;

const DATA: &[(&'static [u8], Op)] = &[
    (b"\x06\x65", ld_r_n(B, 0x65)),
    (b"\x0e\x12", ld_r_n(C, 0x12)),
    (b"\x16\x00", ld_r_n(D, 0x00)),
    (b"\x1e\x08", ld_r_n(E, 0x08)),
    (b"\x26\xa9", ld_r_n(H, 0xa9)),
    (b"\x2e\x99", ld_r_n(L, 0x99)),

    (b"\x7f", ld_r_r(A, A)),
    (b"\x78", ld_r_r(A, B)),
    (b"\x79", ld_r_r(A, C)),
    (b"\x7a", ld_r_r(A, D)),
    (b"\x7b", ld_r_r(A, E)),
    (b"\x7c", ld_r_r(A, H)),
    (b"\x7d", ld_r_r(A, L)),
    (b"\x7e", ld_r_iHL(A)),
    
    (b"\x40", ld_r_r(B, B)),
    (b"\x41", ld_r_r(B, C)),
    (b"\x42", ld_r_r(B, D)),
    (b"\x43", ld_r_r(B, E)),
    (b"\x44", ld_r_r(B, H)),
    (b"\x45", ld_r_r(B, L)),
    (b"\x46", ld_r_iHL(B)),

    (b"\x48", ld_r_r(C, B)),
    (b"\x49", ld_r_r(C, C)),
    (b"\x4a", ld_r_r(C, D)),
    (b"\x4b", ld_r_r(C, E)),
    (b"\x4c", ld_r_r(C, H)),
    (b"\x4d", ld_r_r(C, L)),
    (b"\x4e", ld_r_iHL(C)),

    (b"\x50", ld_r_r(D, B)),
    (b"\x51", ld_r_r(D, C)),
    (b"\x52", ld_r_r(D, D)),
    (b"\x53", ld_r_r(D, E)),
    (b"\x54", ld_r_r(D, H)),
    (b"\x55", ld_r_r(D, L)),
    (b"\x56", ld_r_iHL(D)),

    (b"\x58", ld_r_r(E, B)),
    (b"\x59", ld_r_r(E, C)),
    (b"\x5a", ld_r_r(E, D)),
    (b"\x5b", ld_r_r(E, E)),
    (b"\x5c", ld_r_r(E, H)),
    (b"\x5d", ld_r_r(E, L)),
    (b"\x5e", ld_r_iHL(E)),

    (b"\x31\x0d\x00", ld_rr_nn(SP, 0x000d)),
];

#[test]
fn disassembly() {
    for (data, expected) in DATA.iter() {
        let result = parse_op(data).to_result().unwrap();
        assert_eq!(result, *expected);
        println!("{}", result);
    };
}
