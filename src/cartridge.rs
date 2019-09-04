use std::fs::File;
use std::io::prelude::*;
use std::fmt;
use crate::memory::Memory;

pub struct SnesCartridge {
    memory: Vec<u8>,
    title: String,
    rom_makeup: u8,
    chipset: u8,
    rom_size: usize,
    ram_size: usize,
    country: u8,
    developer_id: u8,
    rom_version: u8,
    checksum_complement: u16,
    checksum: u16,
    ram: Vec<u8>,
}

const WRAM_BANK: u8 = 0x7E;
const WRAM_BANK_END: u8 = 0x7F;

impl SnesCartridge {
    pub fn new(filepath: &str) -> std::io::Result<SnesCartridge> {
        let mut f = File::open(filepath)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        if buffer.len() & 0x3FF == 0x200 {
            buffer = buffer[0x200..].to_vec(); // (extra header from copiers)
        }
        let title_slice = &buffer[0xFFC0 .. 0xFFC0 + 21];
        let title = std::str::from_utf8(title_slice).unwrap().to_string();
        let rom_makeup = buffer[0xFFD5];
        let chipset = buffer[0xFFD6];
        let rom_size = 1024 * (1 << buffer[0xFFD7]);
        let ram_size = 1024 * (1 << buffer[0xFFD8]);
        let country = buffer[0xFFD9];
        let developer_id = buffer[0xFFDA];
        let rom_version = buffer[0xFFDB];
        let checksum_complement =
            (buffer[0xFFDD] as u16) << 8 | buffer[0xFFDC] as u16;
        let checksum = (buffer[0xFFDF] as u16) << 8 | buffer[0xFFDE] as u16;
        let ram = vec![0x0; ram_size];

        Ok(SnesCartridge {
            memory: buffer,
            title: title,
            rom_makeup: rom_makeup,
            chipset: chipset,
            rom_size: rom_size,
            ram_size: ram_size,
            country: country,
            developer_id: developer_id,
            rom_version: rom_version,
            checksum_complement: checksum_complement,
            checksum: checksum,
            ram: ram,
        })
    }

    pub fn rom_file_size(&self) -> usize {
        return self.memory.len();
    }

    fn lorom_read8(&self, bank: u8, offset: u16) -> u8 {
        let addr = ((bank as usize & 0x3f) << 16) | offset as usize;
        let d = self.memory[addr];
        debug!("lorom {:02X}:{:04X} -> {:02X}", bank, offset, d);
        d
    }
    fn hirom_read8(&self, bank: u8, offset: u16) -> u8 {
        let addr = (((bank as usize & 0x7f) - 0x40) << 16) | offset as usize;
        let d = self.memory[addr];
        debug!("rom {:02X}:{:04X}({:06X}) -> {:02X}", bank, offset, addr, d);
        d
    }
    fn ws2_hirom_read8(&self, bank: u8, offset: u16) -> u8 {
        let bank = bank as usize - 0xfe + 0x3e;
        let addr = (bank << 16) | offset as usize;
        let d = self.memory[addr];
        debug!("ws2_hirom {:02X}:{:04X} -> {:02X}", bank, offset, d);
        d
    }
    fn wram_read8(&self, offset: u16) -> u8 {
        let offset = offset as usize;
        let d = self.ram[offset];
        debug!("ram {:04X} -> {:02X}", offset, d);
        d
    }
    fn wram_write8(&mut self, bank: u8, offset: u16, data: u8) {
        self.ram[((bank as usize - WRAM_BANK as usize) << 16) + offset as usize] = data
    }
}

impl Memory for SnesCartridge {
    fn read8(&self, bank: u8, offset: u16) -> u8 {
        match (bank, offset) {
            (0x00 ..= 0x3f, 0x8000 ..= 0xffff) => {
                self.lorom_read8(bank, offset)
            },
            (0x80 ..= 0xbf, 0x8000 ..= 0xffff) => {
                self.lorom_read8(bank, offset)
            },
            // hirom has some mirroring in ram
            (0x20 ..= 0x3f, 0x6000 ..= 0x7fff) => {
                self.wram_read8(offset & 0x1fff)
            },
            (0xa0 ..= 0xbf, 0x6000 ..= 0x7fff) => {
                self.wram_read8(offset & 0x1fff)
            },
            (0x40 ..= 0x7d, _) => {
                self.hirom_read8(bank, offset)
            },
            (0xc0 ..= 0xfd, _) => {
                self.hirom_read8(bank, offset)
            },
            // normal ram read
            (0x7e ..= 0x7f, _) => {
                self.wram_read8(offset)
            },
            (0xfe ..= 0xff, _) => {
                self.ws2_hirom_read8(bank, offset)
            },
            _ => panic!("Unmapped cartridge read {}:{}", bank, offset),
        }
    }

    fn write8(&mut self, bank: u8, offset: u16, data: u8) {
        match (bank, offset) {
            (0x00 ..= 0x3f, 0x8000 ..= 0xffff) => panic!("Write to ROM {}:{}", bank, offset),
            (0x40 ..= 0x7d, 0x0000 ..= 0xffff) => panic!("Write to ROM {}:{}", bank, offset),
            (WRAM_BANK ..= WRAM_BANK_END, 0x0000 ..= 0xffff) => {
                self.wram_write8(bank, offset, data)
            },
            (0x80 ..= 0xbf, 0x8000 ..= 0xffff) => panic!("Write to ROM {}:{}", bank, offset),
            (0xc0 ..= 0xff, 0x0000 ..= 0xffff) => panic!("Write to ROM {}:{}", bank, offset),
            _ => panic!("Unmapped cartridge write {}:{}", bank, offset),
        }
    }

    fn read16(&self, bank: u8, offset: u16) -> u16 {
        (self.read8(bank, offset + 1) << 8) as u16 | self.read8(bank, offset) as u16
    }

    fn write16(&mut self, bank: u8, offset: u16, data: u16) {
        self.write8(bank, offset, (data & 0xFF) as u8);
        self.write8(bank, offset + 1, (data >> 8) as u8);
    }
}

impl fmt::Display for SnesCartridge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
"
  title: {}
  rom_makeup: {:X}
  chipset: {:X}
  rom_size: {}
  ram_size: {}
  country: {:X}
  : {:X}
  rom_version: {}
  checksum_complement: {:X}
  checksum: {:X}",
            self.title,
            self.rom_makeup,
            self.chipset,
            self.rom_size,
            self.ram_size,
            self.country,
            self.developer_id,
            self.rom_version,
            self.checksum_complement,
            self.checksum)
    }
}

