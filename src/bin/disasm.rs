extern crate golbat;

use std::io::{self, Read};
use golbat::cpu::MemoryMap;
use golbat::cpu::asm::Op;

struct Mapper<'a>(&'a mut [u8]);

impl<'a> MemoryMap for Mapper<'a> {
    fn read_u8(&self, offset: u16) -> u8 {
        self.0[offset as usize]
    }

    fn write_u8(&mut self, offset: u16, value: u8) {
        self.0[offset as usize] = value
    }
}

fn main() {
    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer).unwrap();
    let len = buffer.len();
    let mapper = Mapper(&mut buffer);

    let mut pc: u16 = 0;
    loop {
        let (op, npc) = Op::parse(&mapper, pc).unwrap();
        println!("{}", op);
        pc = npc;
        if pc as usize >= len {
            break;
        }
    }
}
