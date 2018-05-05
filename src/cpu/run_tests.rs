use super::*;
use self::asm::Op::*;
use self::asm::op::Reg8::*;

struct TestMapper(Vec<u8>);

impl TestMapper {
    fn new() -> TestMapper {
        TestMapper(vec![0x0; 0x10000])
    }
}

impl Mapper for TestMapper {
    fn read_u8(&self, offset: u16) -> u8 {
        self.0[offset as usize]
    }

    fn write_u8(&mut self, offset: u16, value: u8) {
        self.0[offset as usize] = value
    }
}

#[test]
fn run_ld() {
    let mut mapper = TestMapper::new();
    let mut registers = Registers::default();

    // ld A, $bb
    assert_eq!(run_op(&mut registers, &mut mapper,
                      ld_r_n(A, 0xbb)), Some(8));
    assert_eq!(registers.a, 0xbb);

    // ld L, A
    assert_eq!(run_op(&mut registers, &mut mapper,
                      ld_r_r(L, A)), Some(4));
    assert_eq!(registers.l, registers.a);

    // ld A, (HL)
    assert_eq!(run_op(&mut registers, &mut mapper,
                      ld_r_iHL(A)), Some(8));
    assert_eq!(registers.a, 0x00);
}
