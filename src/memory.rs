
pub trait Memory {
    fn read8(&self, bank: u8, offset: u16) -> u8;
    fn write8(&mut self, bank: u8, offset: u16, data: u8);
    fn read16(&self, bank: u8, offset: u16) -> u16;
    fn write16(&mut self, bank: u8, offset: u16, data: u16);
}

