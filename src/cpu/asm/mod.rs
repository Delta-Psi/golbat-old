mod op;
pub use self::op::{Op, Reg8, Reg16};

mod disasm;
pub use self::disasm::{parse_op, ParseError};

mod display;

#[cfg(test)]
mod tests;
