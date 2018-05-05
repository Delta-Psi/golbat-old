pub trait Mapper {
    fn read_u8(&self, offset: u16) -> u8;
    fn write_u8(&mut self, offset: u16, value: u8);

    fn read_u16(&self, _offset: u16) -> u16 {
        unimplemented!()
    }

    fn write_u16(&mut self, _offset: u16, _value: u16) {
        unimplemented!()
    }
}
