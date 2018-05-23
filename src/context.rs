use super::*;
use cpu::{run_op, Registers};
use cpu::asm::{Op, ParseError};
use std::time::Duration;

const CLOCK_SPEED: u64 = 4_194_304; // Hz

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
        self.cpu.pc = 0x0100;
    }

    pub fn step(&mut self) -> Result<(Op, Duration), Error> {
        let (op, pc) = Op::parse(&self.memory, self.cpu.pc).map_err(Error::OpParseError)?;
        println!("{}", op);
        self.cpu.pc = pc;
        let cycles = run_op(&mut self.cpu, &mut self.memory, op).unwrap(); // FIXME
        let time = Duration::new(0, (f32::from(cycles) * 1_000_000_000.0 / CLOCK_SPEED as f32) as u32);
        Ok((op, time))
    }
}
