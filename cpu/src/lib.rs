#[macro_use]
extern crate bitflags;

pub mod asm;

mod registers;
pub use registers::{Flags, Registers};

mod mapper;
pub use mapper::MemoryMap;

mod run;
pub use run::run_op;

#[cfg(test)]
mod run_tests;
