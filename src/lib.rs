#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate nom;

pub mod cpu;

mod rom;
pub use rom::{Header, Rom};

mod mapper;
pub use mapper::Mapper;
