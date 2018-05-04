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
