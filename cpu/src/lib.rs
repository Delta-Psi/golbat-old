#[macro_use]
extern crate bitflags;

pub mod asm;

mod registers;
pub use registers::{Flags, Registers};

mod mapper;
pub use mapper::MemoryMap;

mod run;

#[cfg(test)]
mod run_tests;
