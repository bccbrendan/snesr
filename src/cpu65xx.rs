pub struct Cpu65xx {
    // reference: https://problemkaputt.de/fullsnes.htm#cpu65xxmicroprocessor
    a: u16,  // accumulator
    x: u16,  // index register x
    y: u16,  // index register y
    pc: u16, // program counter
    s: u16,  // stack pointer. points to the first _free_ byte in page 1.
    p: u8,   // processor status register
    d: u16,  // zeropage offset ;expands 8bit [nn] to 16bit [00:nn+D]
    db: u8,  // data bank ;expands 16bit [nnnn] to 24bit [DB:nnnn]
    pb: u8,  // program counter bank ;expands 16bit PC to 24bit PB:PC
    e: bool, // emulation flag for 6502 emulation
}

impl Cpu65xx {
    pub fn new() -> Cpu65xx {
        Cpu65xx {
            a: 0,  
            x: 0,  
            y: 0,  
            pc: 0, 
            s: 0,  
            p: 0,   
            d: 0,  
            db: 0,  
            pb: 0,  
            e: true
        }
    }

    pub fn reset(&mut self) {
        self.p =  1 << 2; // D = 0, I=1
        self.pc = 0xFFFC;
        self.s = 0x00FF;
        self.d = 0x0000;
        self.db = 0x00;
        self.pb = 0x00;
        self.e = true;
    }

}

