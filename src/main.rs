use std::env;
use std::io::prelude::*;
use std::fs::File;

fn main() -> std::io::Result<()> {
    println!("Hello, snesr!");
    let args: Vec<String> = env::args().collect();
    let rom_file = &args[1];
    println!("opening {}", rom_file);
    let mut f = File::open(rom_file)?;

    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;
    println!("read {} bytes", buffer.len());
    Ok(())
}
