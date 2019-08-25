use std::io::prelude::*;
use std::fs::File;

pub struct SnesCartridge {
    memory: Vec<u8>,
    header_offset: u32,
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
}

impl SnesCartridge {
    pub fn new(filepath: &str) -> std::io::Result<SnesCartridge> {
        let mut f = File::open(filepath)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        let mut offset = 0; // TODO support LoRom and ExHiRom
        if buffer.len() & 0x3FF == 0x200 {
            offset += 0x200; // (extra header from copiers)
        }
        let title_slice = &buffer[0xFFC0 + offset..0xFFC0 + offset + 21];
        let title = std::str::from_utf8(title_slice).unwrap().to_string();
        let rom_makeup = buffer[0xFFD5 + offset];
        let chipset = buffer[0xFFD6 + offset];
        let rom_size = 1024 * (1 << buffer[0xFFD7 + offset]);
        let ram_size = 1024 * (1 << buffer[0xFFD8 + offset]);
        let country = buffer[0xFFD9 + offset];
        let developer_id = buffer[0xFFDA + offset];
        let rom_version = buffer[0xFFDB + offset];
        let checksum_complement = (buffer[0xFFDD + offset] as u16) << 8 | buffer[0xFFDC + offset] as u16;
        let checksum = (buffer[0xFFDF + offset] as u16) << 8 | buffer[0xFFDE + offset] as u16;
        Ok(SnesCartridge {
            memory: buffer,
            header_offset: offset as u32,
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
        })
    }

    pub fn rom_file_size(&self) -> usize {
        return self.memory.len()
    }

    pub fn print_info(&self) {
        println!("Rom title: {}", self.title);
        println!("rom_makeup: {:X}", self.rom_makeup);
        println!("chipset: {:X}", self.chipset);
        println!("rom_size: {}", self.rom_size);
        println!("ram_size: {}", self.ram_size);
        println!("country: {:X}", self.country);
        println!("developer_id: {:X}", self.developer_id);
        println!("rom_version: {}", self.rom_version);
        println!("checksum_complement: {:X}", self.checksum_complement);
        println!("checksum: {:X}", self.checksum);
    }
}
