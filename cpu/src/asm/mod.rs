mod op;
pub use self::op::{Op, Reg16, Reg8};

mod disasm;
pub use self::disasm::ParseError;

mod display;

#[cfg(test)]
mod tests;
