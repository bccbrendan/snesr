use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use bitfield::BitRange;
use crate::memory::Memory;

bitfield! {
    struct Status(u8);
    impl Debug;
    u8;
    c, set_c: 0, 0;  // Carry         (0=No Carry, 1=Carry)
    z, set_z: 1, 1;  // Zero          (0=Nonzero, 1=Zero)
    i, set_i: 2, 2;  // IRQ Disable   (0=IRQ Enable, 1=IRQ Disable)
    d, set_d: 3, 3;  // Decimal Mode  (0=Normal, 1=BCD Mode for ADC/SBC opcodes)
    x, set_x: 4, 4;  // Break Flag    (0=IRQ/NMI, 1=BRK/PHP opcode)  (0=16bit, 1=8bit)
    m, set_m: 5, 5;  // Unused        (Always 1)                     (0=16bit, 1=8bit)
    v, set_v: 6, 6;  // Overflow      (0=No Overflow, 1=Overflow)
    n, set_n: 7, 7;  // Negative/Sign (0=Positive, 1=Negative)
}
impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}{}{}{}{}{}",
            if self.n()!=0 {'N'} else {'-'},
            if self.v()!=0 {'V'} else {'-'},
            if self.m()!=0 {'M'} else {'-'},
            if self.x()!=0 {'X'} else {'-'},
            if self.d()!=0 {'D'} else {'-'},
            if self.i()!=0 {'I'} else {'-'},
            if self.z()!=0 {'Z'} else {'-'},
            if self.c()!=0 {'C'} else {'-'})
    }
}

pub struct Cpu65xx<M: Memory> {
    // reference: https://problemkaputt.de/fullsnes.htm#cpu65xxmicroprocessor
    a: u16,  // accumulator
    x: u16,  // index register x
    y: u16,  // index register y
    pc: u16, // program counter
    s: u16,  // stack pointer. points to the first _free_ byte in page 1.
    p: Status,   // processor status register
    d: u16,  // zeropage offset ;expands 8bit [nn] to 16bit [00:nn+D]
    db: u8,  // data bank ;expands 16bit [nnnn] to 24bit [DB:nnnn]
    pb: u8,  // program counter bank ;expands 16bit PC to 24bit PB:PC
    e: bool, // emulation flag for 6502 emulation

    mem: Rc<RefCell<M>>,
}

pub const RESET_VECTOR: u16 = 0xFFFC;

impl<M: Memory> Cpu65xx<M> {
    pub fn new(mem: Rc<RefCell<M>>) -> Cpu65xx<M> {
        Cpu65xx {
            a: 0,  
            x: 0,  
            y: 0,  
            pc: 0, 
            s: 0,  
            p: Status(0),
            d: 0,  
            db: 0,  
            pb: 0,  
            e: true,
            mem: mem,
        }
    }

    pub fn reset(&mut self) {
        self.p.set_bit_range(7, 0, 0);
        self.p.set_m(1);
        self.pc = self.read16(0, RESET_VECTOR);
        self.s = 0x00FF;
        self.d = 0x0000;
        self.db = 0x00;
        self.pb = 0x00;
        self.e = true;
    }

    pub fn set_nz_flags(&mut self, val: u16) {
        let (n_mask, z_mask) = match self.e {
            true => (0xFF, 0x80),
            _ => (0xFFFF, 0x8000),
        };
        self.p.set_n(match val & n_mask {
            0x0000 => 0,  // value is not negative
            _ => 1,
        });
        self.p.set_z(match val & z_mask {
            0x0000 => 1, // value is 0
            _ => 0,
        });
    }

    pub fn set_a_with_nz(&mut self, val: u16) {
        let val_mask = match self.e {
            true => 0xFF,
            _ => 0xFFFF,
        };
        self.a = val & val_mask;
        self.set_nz_flags(self.a);
    }

    pub fn set_x_with_nz(&mut self, val: u16) {
        let val_mask = match self.e {
            true => 0xFF,
            _ => 0xFFFF,
        };
        self.x = val & val_mask;
        self.set_nz_flags(self.x);
    }

     pub fn set_y_with_nz(&mut self, val: u16) {
        let val_mask = match self.e {
            true => 0xFF,
            _ => 0xFFFF,
        };
        self.y = val & val_mask;
        self.set_nz_flags(self.y);
    }
 
    pub fn decode(&mut self) -> usize {
        let prev_pc = self.pc;
        let opcode = self.fetch8();
        debug!("PC: {:02X}{:04X} {}{} opcode: {:02X}",
            self.pb, prev_pc, self.p, if self.e {'e'} else {'_'}, opcode);
        match opcode {
            //== CPU Memory and Register Transfers
            // Register to Register Transfer
            //A8        nz----  2   TAY     MOV Y,A   x     Y=A
            //AA        nz----  2   TAX     MOV X,A   x     X=A
            //BA        nz----  2   TSX     MOV X,S   x     X=S
            //98        nz----  2   TYA     MOV A,Y   m     A=Y
            //8A        nz----  2   TXA     MOV A,X   m     A=X
            //9A        ------  2   TXS     MOV S,X   e     S=X
            //9B        nz----  2   TXY     MOV Y,X   x     Y=X
            //BB        nz----  2   TYX     MOV X,Y   x     X=Y
            //7B        nz----  2   TDC     MOV A,D   16    A=D
            //5B        nz----  2   TCD     MOV D,A   16    D=A
            //3B        nz----  2   TSC     MOV A,SP  16    A=SP
            //1B        ------  2   TCS     MOV SP,A  e?    SP=A
            // Load Register from Memory
            // * Add one cycle if indexing crosses a page boundary.
            //A9 nn       nz----  2   LDA #nn      MOV A,nn          A=nn
            //A5 nn       nz----  3   LDA nn       MOV A,[nn]        A=[D+nn]
            //B5 nn       nz----  4   LDA nn,X     MOV A,[nn+X]      A=[D+nn+X]
            //A3 nn       nz----      LDA nn,S     MOV A,[nn+S]      A=[nn+S]
            //AD nn nn    nz----  4   LDA nnnn     MOV A,[nnnn]      A=[DB:nnnn]
            //BD nn nn    nz----  4*  LDA nnnn,X   MOV A,[nnnn+X]    A=[DB:nnnn+X]
            //B9 nn nn    nz----  4*  LDA nnnn,Y   MOV A,[nnnn+Y]    A=[DB:nnnn+Y]
            //AF nn nn nn nz----      LDA nnnnnn   MOV A,[nnnnnn]    A=[nnnnnn]
            //BF nn nn nn nz----      LDA nnnnnn,X MOV A,[nnnnnn+X]  A=[nnnnnn+X]
            //B2 nn       nz----      LDA (nn)     MOV A,[[nn]]      A=[WORD[D+nn]]
            //A1 nn       nz----  6   LDA (nn,X)   MOV A,[[nn+X]]    A=[WORD[D+nn+X]]
            //B1 nn       nz----  5*  LDA (nn),Y   MOV A,[[nn]+Y]    A=[WORD[D+nn]+Y]
            //B3 nn       nz----      LDA (nn,S),Y MOV A,[[nn+S]+Y]  A=[WORD[nn+S]+Y]
            //A7 nn       nz----      LDA [nn]     MOV A,[FAR[nn]]   A=[FAR[D+nn]]
            //B7 nn       nz----      LDA [nn],y   MOV A,[FAR[nn]+Y] A=[FAR[D+nn]+Y]
            //A2 nn       nz----  2   LDX #nn      MOV X,nn          X=nn
            0xA2 => { let o = self.fetch8(); self.ldx(o as u16); 2 }
            //A6 nn       nz----  3   LDX nn       MOV X,[nn]        X=[D+nn]
            //B6 nn       nz----  4   LDX nn,Y     MOV X,[nn+Y]      X=[D+nn+Y]
            //AE nn nn    nz----  4   LDX nnnn     MOV X,[nnnn]      X=[DB:nnnn]
            //BE nn nn    nz----  4*  LDX nnnn,Y   MOV X,[nnnn+Y]    X=[DB:nnnn+Y]
            //A0 nn       nz----  2   LDY #nn      MOV Y,nn          Y=nn
            //A4 nn       nz----  3   LDY nn       MOV Y,[nn]        Y=[D+nn]
            //B4 nn       nz----  4   LDY nn,X     MOV Y,[nn+X]      Y=[D+nn+X]
            //AC nn nn    nz----  4   LDY nnnn     MOV Y,[nnnn]      Y=[DB:nnnn]
            //BC nn nn    nz----  4*  LDY nnnn,X   MOV Y,[nnnn+X]    Y=[DB:nnnn+X]
            // Store Register in Memory
            // 64 nn       ------  3   STZ nn       MOV [nn],0        m  [D+nn]=0
            // 74 nn       ------  4   STZ nn_x     MOV [nn+X],0      m  [D+nn+X]=0
            // 9C nn nn    ------  4   STZ nnnn     MOV [nnnn],0      m  [DB:nnnn]=0
            // 9E nn nn    ------  5   STZ nnnn_x   MOV [nnnn+X],0    m  [DB:nnnn+X]=0
            // 85 nn       ------  3   STA nn       MOV [nn],A        m  [D+nn]=A
            // 95 nn       ------  4   STA nn,X     MOV [nn+X],A      m  [D+nn+X]=A
            // 83 nn       ------      STA nn,S     MOV [nn+S],A      m  [nn+S]=A
            // 8D nn nn    ------  4   STA nnnn     MOV [nnnn],A      m  [DB:nnnn]=A
            // 9D nn nn    ------  5   STA nnnn,X   MOV [nnnn+X],A    m  [DB:nnnn+X]=A
            // 99 nn nn    ------  5   STA nnnn,Y   MOV [nnnn+Y],A    m  [DB:nnnn+Y]=A
            // 8F nn nn nn ------      STA nnnnnn   MOV [nnnnnn],A    m  [nnnnnn]=A
            // 9F nn nn nn ------      STA nnnnnn,X MOV [nnnnnn+X],A  m  [nnnnnn+X]=A
            // 81 nn       ------  6   STA (nn,X)   MOV [[nn+X]],A    m  [WORD[D+nn+X]]=A
            // 91 nn       ------  6   STA (nn),Y   MOV [[nn]+Y],A    m  [WORD[D+nn]+Y]=A
            // 92 nn       ------      STA (nn)     MOV [[nn]],A      m  [WORD[D+nn]]=A
            // 93 nn       ------      STA (nn,S),Y MOV [[nn+S]+Y],A  m  [WORD[nn+S]+Y]=A
            // 87 nn       ------      STA [nn]     MOV [FAR[nn]],A   m  [FAR[D+nn]]=A
            // 97 nn       ------      STA [nn],y   MOV [FAR[nn]+Y],A m  [FAR[D+nn]+Y]=A
            // 86 nn       ------  3   STX nn       MOV [nn],X        x  [D+nn]=X
            // 96 nn       ------  4   STX nn,Y     MOV [nn+Y],X      x  [D+nn+Y]=X
            // 8E nn nn    ------  4   STX nnnn     MOV [nnnn],X      x  [DB:nnnn]=X
            // 84 nn       ------  3   STY nn       MOV [nn],Y        x  [D+nn]=Y
            // 94 nn       ------  4   STY nn,X     MOV [nn+X],Y      x  [D+nn+X]=Y
            // 8C nn nn    ------  4   STY nnnn     MOV [nnnn],Y      x  [DB:nnnn]=Y
            // Push/Pull (Stack)
            // 48        ------  3   PHA       PUSH A          m  [S]=A
            // DA        ------  3   PHX       PUSH X          x  [S]=X
            // 5A        ------  3   PHY       PUSH Y          x  [S]=Y
            // 08        ------  3   PHP       PUSH P          8  [S]=P
            // 8B        ------  3   PHB       PUSH DB         8  [S]=DB
            // 4B        ------  3   PHK       PUSH PB         8  [S]=PB
            // 0B        ------  4   PHD       PUSH D         16  [S]=D
            // D4 nn     ------  6   PEI nn    PUSH WORD[nn]  16  [S]=WORD[D+nn]
            // F4 nn nn  ------  5   PEA nnnn  PUSH nnnn      16  [S]=NNNN
            // 62 nn nn  ------  6   PER rel16 PUSH disp16    16  [S]=$+3+disp
            // 68        nz----  4   PLA       POP  A          m  A=[S]
            // FA        nz----  4   PLX       POP  X          x  X=[S]
            // 7A        nz----  4   PLY       POP  Y          x  Y=[S]
            // 2B        nz----  5   PLD       POP  D         16  D=[S]
            // AB        nz----  4   PLB       POP  DB         8  DB=[S]
            // 28        nzcidv  4   PLP       POP  P          8  P=[S]
            // Memory Block Transfer Commands
            // 44 dd ss  ------  7x  MVP ss,dd   LDDR [dd:Y],[ss:X],A+1  ;DEC X/Y
            // 54 dd ss  ------  7x  MVN ss,dd   LDIR [dd:Y],[ss:X],A+1  ;INC X/Y

            // == CPU Rotate and Shift Instructions
            // Shift Left Logical/Arithmetic
            //  0A        nzc---  2   ASL A       SHL A         SHL A
            0x06 => { let op = self.zeropage8(); self.asl(Some((0, op.into()))); 5 },
            //  16 nn     nzc---  6   ASL nn,X    SHL [nn+X]    SHL [D+nn+X]
            //  0E nn nn  nzc---  6   ASL nnnn    SHL [nnnn]    SHL [DB:nnnn]
            //  1E nn nn  nzc---  7   ASL nnnn,X  SHL [nnnn+X]  SHL [DB:nnnn+X]
            //  4A        0zc---  2   LSR A       SHR A         SHR A
            //  46 nn     0zc---  5   LSR nn      SHR [nn]      SHR [D+nn]
            //  56 nn     0zc---  6   LSR nn,X    SHR [nn+X]    SHR [D+nn+X]
            //  4E nn nn  0zc---  6   LSR nnnn    SHR [nnnn]    SHR [DB:nnnn]
            //  5E nn nn  0zc---  7   LSR nnnn,X  SHR [nnnn+X]  SHR [DB:nnnn+X]
            //  2A        nzc---  2   ROL A       RCL A         RCL A
            //  26 nn     nzc---  5   ROL nn      RCL [nn]      RCL [D+nn]
            //  36 nn     nzc---  6   ROL nn,X    RCL [nn+X]    RCL [D+nn+X]
            //  2E nn nn  nzc---  6   ROL nnnn    RCL [nnnn]    RCL [DB:nnnn]
            //  3E nn nn  nzc---  7   ROL nnnn,X  RCL [nnnn+X]  RCL [DB:nnnn+X]
            //  6A        nzc---  2   ROR A       RCR A         RCR A
            //  66 nn     nzc---  5   ROR nn      RCR [nn]      RCR [D+nn]
            //  76 nn     nzc---  6   ROR nn,X    RCR [nn+X]    RCR [D+nn+X]
            //  6E nn nn  nzc---  6   ROR nnnn    RCR [nnnn]    RCR [DB:nnnn]
            //  7E nn nn  nzc---  7   ROR nnnn,X  RCR [nnnn+X]  RCR [DB:nnnn+X]
            // == CPU Jump and Control Instructions
            // Normal Jumps
            //  80 dd        ------  3xx BRA disp8    JMP disp      PC=PC+/-disp8
            //  82 dd dd     ------  4   BRL disp16   JMP disp      PC=PC+/-disp16
            //  4C nn nn     ------  3   JMP nnnn     JMP nnnn      PC=nnnn
            0x5C => { let pc = self.fetch16(); let pb = self.fetch8(); self.jmp(pb, pc); 4 },
            //  6C nn nn     ------  5   JMP (nnnn)   JMP [nnnn]    PC=WORD[00:nnnn]
            //  7C nn nn     ------  6   JMP (nnnn,X) JMP [nnnn+X]  PC=WORD[PB:nnnn+X]
            //  DC nn nn     ------  6   JML ...      JMP FAR[nnnn] PB:PC=[00:nnnn]
            //  20 nn nn     ------  6   JSR nnnn     CALL nnnn     [S]=PC+2,PC=nnnn
            //  22 nn nn nn  ------  4   JSL nnnnnn   CALL nnnnnn   PB:PC=nnnnnn [S]=PB:PC+3
            //  FC nn nn     ------  6   JSR (nnnn,X) CALL [nnnn+X] PC=WORD[PB:nnnn+X] [S]=PC
            //0x40 => self.rti(),
            ////0x6B => self.rtl(),
            //0x60 => self.rts(),

            // CPU Control
            0x18 => { self.clc(); 2 },
            0x58 => { self.cli(); 2 },
            0xd8 => { self.cld(); 2 },
            0xb8 => { self.clv(); 2 },
            0x38 => { self.sec(); 2 },
            0x78 => { self.sei(); 2 },
            0xf8 => { self.sed(); 2 },
            0xc2 => { let o = self.fetch8(); self.rep(o); 3 },
            0xE2 => { let o = self.fetch8(); self.sep(o); 3 },
            0xFB => { self.xce(); 2 },
            _ => panic!("unsupported opcode: {:X}", opcode)
        }
    }

    fn fetch8(&mut self) -> u8 {
        let pc = self.pc;
        let data = self.read8(self.pb, pc);
        self.pc = self.pc.wrapping_add(1);
        data
    }
    fn fetch16(&mut self) -> u16 {
        let pc = self.pc;
        let data = self.read16(self.pb, pc);
        self.pc = self.pc.wrapping_add(2);
        data
    }

    fn zeropage8(&mut self) -> u8 {
        let addr = self.fetch8();
        let data = self.read8(0, addr as u16);
        self.pc = self.pc.wrapping_add(1);
        data
    }

    fn read8(&self, bank: u8, offset: u16) -> u8 { self.mem.borrow_mut().read8(bank, offset) } 
    fn write8(&self, bank: u8, offset: u16, data: u8) { self.mem.borrow_mut().write8(bank, offset, data) } 
    fn read16(&self, bank: u8, offset: u16) -> u16 { self.mem.borrow_mut().read16(bank, offset) } 
    fn write16(&self, bank: u8, offset: u16, data: u16) { self.mem.borrow_mut().write16(bank, offset, data) } 

    //== CPU Memory and Register Transfers
    // Register to Register Transfer
    //A8        nz----  2   TAY     MOV Y,A   x     Y=A
    //AA        nz----  2   TAX     MOV X,A   x     X=A
    //BA        nz----  2   TSX     MOV X,S   x     X=S
    //98        nz----  2   TYA     MOV A,Y   m     A=Y
    //8A        nz----  2   TXA     MOV A,X   m     A=X
    //9A        ------  2   TXS     MOV S,X   e     S=X
    //9B        nz----  2   TXY     MOV Y,X   x     Y=X
    //BB        nz----  2   TYX     MOV X,Y   x     X=Y
    //7B        nz----  2   TDC     MOV A,D   16    A=D
    //5B        nz----  2   TCD     MOV D,A   16    D=A
    //3B        nz----  2   TSC     MOV A,SP  16    A=SP
    //1B        ------  2   TCS     MOV SP,A  e?    SP=A
    /* Load Register from Memory */
    fn lda(&mut self, value: u16) { debug!("LDA {:04X}", value); self.set_a_with_nz(value) }
    fn ldx(&mut self, value: u16) { debug!("LDX {:04X}", value); self.set_x_with_nz(value) }
    fn ldy(&mut self, value: u16) { debug!("LDY {:04X}", value); self.set_y_with_nz(value) }

    // == CPU Rotate and Shift Instructions
    // ASL Arithmetic Shift Left
    fn asl(&mut self, addr: Option<(u8, u16)>) {
        let old = match addr {
            None => {
                debug!("ASL A");
                self.a
            },
            Some((bank, offset)) => {
                debug!("ASL {:02X}{:04X}", bank, offset);
                match self.e {
                    true => self.read8(bank, offset) as u16,
                    false => self.read16(bank, offset),
                }
            },
        };
        let carry = match self.e {
            true => old & 0x80 != 0,
            false => old & 0x8000 != 0,
        };
        let (new_val, _) = self.a.overflowing_shl(1 as u32);
        self.p.set_c(if carry {0} else {1});
        match addr {
            None => {
                self.a = new_val;
            },
            Some((bank, offset)) => {
                match self.e {
                    true => self.write8(bank, offset, (new_val & 0xFF) as u8),
                    false => self.write16(bank, offset, new_val),
                }
            },
        }
    }

    // LSR Shift Right Logical
    // ROL Rotate Left through Carry
    // ROR Rotate Right through Carry
    /* Normal Jumps
      Opcode       Flags Clks  Native       Nocash        Effect
      80 dd        ------  3xx BRA disp8    JMP disp      PC=PC+/-disp8
      82 dd dd     ------  4   BRL disp16   JMP disp      PC=PC+/-disp16
      4C nn nn     ------  3   JMP nnnn     JMP nnnn      PC=nnnn
      5C nn nn nn  ------  4   JMP nnnnnn   JMP nnnnnn    PB:PC=nnnnnn
      6C nn nn     ------  5   JMP (nnnn)   JMP [nnnn]    PC=WORD[00:nnnn]
      7C nn nn     ------  6   JMP (nnnn,X) JMP [nnnn+X]  PC=WORD[PB:nnnn+X]
      DC nn nn     ------  6   JML ...      JMP FAR[nnnn] PB:PC=[00:nnnn]
      20 nn nn     ------  6   JSR nnnn     CALL nnnn     [S]=PC+2,PC=nnnn
      22 nn nn nn  ------  4   JSL nnnnnn   CALL nnnnnn   PB:PC=nnnnnn [S]=PB:PC+3
      FC nn nn     ------  6   JSR (nnnn,X) CALL [nnnn+X] PC=WORD[PB:nnnn+X] [S]=PC
      40           nzcidv  6   RTI          RETI          P=[S+1],PB:PC=[S+2],S=S+4
      6B           ------  ?   RTL          RETF          PB:PC=[S+1]+1, S=S+3
      60           ------  6   RTS          RET           PC=[S+1]+1, S=S+2
    */
    fn jmp(&mut self, pb: u8, pc: u16) {
        self.pb = pb;
        self.pc = pc;
        debug!("JMP 0x{:02X}{:04X}", pb, pc);
    }
    fn clc(&mut self) {
        debug!("CLC");
        self.p.set_c(0);
    }
    fn cli(&mut self) {
        debug!("CLI");
        self.p.set_i(0);
    }
    fn cld(&mut self) {
        debug!("CLD");
        self.p.set_d(0);
    }
    fn clv(&mut self) {
        debug!("CLV");
        self.p.set_v(0);
    }
    fn sec(&mut self) {
        debug!("SEC");
        self.p.set_c(1);
    }
    fn sei(&mut self) {
        debug!("SEI");
        self.p.set_i(1);
    }
    fn sed(&mut self) {
        debug!("SED");
        self.p.set_d(1);
    }
    fn rep(&mut self, operand: u8) {
        debug!("REP {:02X}", operand);
        let old_p : u8 = self.p.bit_range(7, 0);
        self.p.set_bit_range(7, 0, old_p & !operand);
    }
    fn sep(&mut self, operand: u8) {
        debug!("SEP {:02X}", operand);
        let old_p : u8 = self.p.bit_range(7, 0);
        self.p.set_bit_range(7, 0, old_p | operand);
    }
    fn xce(&mut self) {
        debug!("XCE");
        let old_e = self.e;
        self.e = match self.p.c() { 1 => true, 0 => false, _ => panic!() };
        self.p.set_c(match old_e { true => 1, false => 0 });
    }
}

