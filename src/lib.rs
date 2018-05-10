#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate nom;

pub mod cpu;

mod mapper;
pub use mapper::Mapper;
