pub trait MemoryMap {
    fn read_u8(&self, offset: u16) -> u8;
    fn write_u8(&mut self, offset: u16, value: u8);

    fn read_i8(&self, offset: u16) -> i8 {
        self.read_u8(offset) as i8
    }

    fn write_i8(&mut self, offset: u16, value: i8) {
        self.write_u8(offset, value as u8)
    }

    fn read_u16(&self, offset: u16) -> u16 {
        ((self.read_u8(offset+1) as u16) << 8) | (self.read_u8(offset) as u16)
    }

    fn write_u16(&mut self, _offset: u16, _value: u16) {
        unimplemented!()
    }
}
