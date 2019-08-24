
use crate::cpu65xx::Cpu65xx;

pub struct Snes {
    cpu: Cpu65xx,
}

impl Snes {
    pub fn new(cpu: Cpu65xx) -> Snes {
        Snes {
            cpu,
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
    }

    pub fn emulate_frame(&mut self) {
    }
}
