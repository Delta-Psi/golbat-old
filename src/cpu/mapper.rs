use std::ops::IndexMut;

pub trait Mapper: IndexMut<u16, Output = u8> {
}

