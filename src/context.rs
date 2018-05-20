use super::*;
use cpu::Registers;

#[derive(Debug)]
pub struct Context {
    pub cpu: Registers,
    pub memory: Mapper,
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
}
