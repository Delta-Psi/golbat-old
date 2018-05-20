use super::*;
use cpu::{run_op, Registers};
use cpu::asm::{Op, ParseError};

#[derive(Debug)]
pub struct Context {
    pub cpu: Registers,
    pub memory: Mapper,
}

#[derive(Debug)]
pub enum Error {
    OpParseError(ParseError),
}

impl Context {
    pub fn new(rom: Rom) -> Context {
        Context {
            cpu: Registers::default(),
            memory: Mapper::new(rom),
        }
    }

    pub fn bootstrap(&mut self) {
        self.cpu.set_af(0x01b0);
        self.cpu.set_bc(0x0013);
        self.cpu.set_de(0x00d8);
        self.cpu.set_hl(0x014d);
        self.cpu.sp = 0xfffe;
    }

    pub fn step(&mut self) -> Result<(Op, u8), Error> {
        let (op, pc) = Op::parse(&mut self.memory, self.cpu.pc)
            .map_err(Error::OpParseError)?;
        self.cpu.pc = pc;
        Ok((op, run_op(&mut self.cpu, &mut self.memory, op).unwrap())) // FIXME
    }
}
