use crate::cartridge::SnesCartridge;

use crate::memory::Memory;

pub struct Peripherals {
    cartridge: SnesCartridge,
    wram: Vec<u8>,
    // apu, ppu, controllers
}

const WRAM_SIZE: usize = 128 * 1024;

impl Peripherals {
    pub fn new(cartridge: SnesCartridge) -> Peripherals {
        let wram = vec![0x0; WRAM_SIZE];
        Peripherals {
            cartridge: cartridge,
            wram: wram,
        }
    }


    fn system_area_read8(&self, bank: u8, offset: u16) -> u8 {
        /*
          System Area (banks 00h-3Fh and 80h-BFh)
          Offset       Content                                              Speed
          0000h-1FFFh  Mirror of 7E0000h-7E1FFFh (first 8Kbyte of WRAM)     2.68MHz
          2000h-20FFh  Unused                                               3.58MHz
          2100h-21FFh  I/O Ports (B-Bus)                                    3.58MHz
          2200h-3FFFh  Unused                                               3.58MHz
          4000h-41FFh  I/O Ports (manual joypad access)                     1.78MHz
          4200h-5FFFh  I/O Ports                                            3.58MHz
          6000h-7FFFh  Expansion                                            2.68MHz
        */
        match (bank, offset) {
             _ => panic!("unimplmented read at offset {0:04X}", offset)
        }
        
    }

    fn system_area_write8(&mut self, bank: u8, offset: u16, data: u8) {
        panic!("unimplemented write: {:X}:{:X}", bank, offset)
    }
}



// reference: https://problemkaputt.de/fullsnes.htm#snesmemorymap
impl Memory for Peripherals {
    // TODO check endianness
    fn read8(&self, bank: u8, offset: u16) -> u8 {
        match (bank, offset) {
            (0x00 ..= 0x3f, 0x0000 ..= 0x1fff) => {
                // work ram mirror
                self.wram[offset as usize]
            },
            (0x80 ..= 0xbf, 0x0000 ..= 0x1fff) => {
                // work ram mirror
                self.wram[offset as usize]
            },
            (0x7E ..= 0x7F, _) => {
                // work ram
                let index = (bank as usize - 0x7e) << 16 | offset as usize;
                self.wram[index as usize]
            }
            _ => self.cartridge.read8(bank, offset),

        }
    }

    fn write8(&mut self, bank: u8, offset: u16, data: u8) {
        match (bank, offset) {
            (0x00 ..= 0x3f, 0x0000 ..= 0x1fff) => {
                // work ram mirror
                self.wram[offset as usize] = data
            },
            (0x80 ..= 0xbf, 0x0000 ..= 0x1fff) => {
                // work ram mirror
                self.wram[offset as usize] = data
            },
            (0x7E ..= 0x7F, _) => {
                // work ram
                let index = (bank as usize - 0x7e) << 16 | offset as usize;
                self.wram[index as usize] = data
            }
            (0x00 ..= 0x3f, 0x8000 ..= 0xffff) => {
                self.cartridge.write8(bank, offset, data)
            },
            _ => panic!("Unmapped write {}:{}", bank, offset)

        }
    }

    fn read16(&self, bank: u8, offset: u16) -> u16 {
        let low = self.read8(bank, offset) as u16;
        let high = self.read8(bank, offset + 1) as u16;
        high << 8 | low
    }

    fn write16(&mut self, bank: u8, offset: u16, data: u16) {
        let low = (data & 0xFF) as u8;
        let high = (data >> 8) as u8;
        self.write8(bank, offset, low);
        self.write8(bank, offset + 1, high)
    }
}
