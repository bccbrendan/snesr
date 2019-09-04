use std::rc::Rc;
use std::cell::RefCell;
use crate::peripherals::Peripherals;
use crate::cpu65xx::Cpu65xx;
use crate::memory::Memory;
pub struct Snes<M: Memory> {
    cpu: Cpu65xx<M>,
    peripherals: Rc<RefCell<Peripherals>>,
}

impl<M: Memory> Snes<M> {
    pub fn new(cpu: Cpu65xx<M>, peripherals: Rc<RefCell<Peripherals>>) -> Snes<M> {
        Snes {
            cpu: cpu,
            peripherals: peripherals,
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
    }

    pub fn emulate_frame(&mut self) {
        let cycles = self.cpu.decode();
    }
}
