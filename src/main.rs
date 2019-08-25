mod snes;
mod cpu65xx;
mod cartridge;

extern crate sdl2;

use std::env;
use std::time::Instant;
use std::time::Duration;
use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use crate::snes::Snes;
use crate::cpu65xx::Cpu65xx;
use crate::cartridge::SnesCartridge;

fn main() -> std::io::Result<()> {
    println!("Hello, snesr!");
    // load ROM file
    let args: Vec<String> = env::args().collect();
    let rom_file = &args[1];
    println!("opening {}", rom_file);
    let mut snes_cart = SnesCartridge::new(rom_file)?;
    println!("read {} bytes", snes_cart.rom_file_size());
    snes_cart.print_info();

    // initialize sdl2 window
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem.window("snesr", 800, 600)
        .position_centered()
        .build()
        .unwrap();
    let mut canvas = window.into_canvas().build().unwrap();
    canvas.set_draw_color(Color::RGB(0, 255, 255));
    canvas.clear();
    canvas.present();
    // initialize sdl2 event loop
    let mut event_pump = sdl_context.event_pump().unwrap();

    // put together the emulated snes hardware
    let cpu = Cpu65xx::new();
    let mut snes = Snes::new(cpu);
    snes.reset();
    let mut i = 0;
    // time each frame ought to take to execute
    let target_time = Duration::from_millis(1000 / 60);
    'game_loop: loop {
        let time_before = Instant::now();
        i = (i + 1) % 255;
        canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        canvas.clear();
        // handle events from SDL
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'game_loop
                },
                _ => {}
            }
        }
        // handle the rest of the game loop 
        snes.emulate_frame();
        canvas.present();
        // sleep to adjust for fps
        let sleep_millis = target_time.checked_sub(Instant::now() - time_before);
        match sleep_millis {
            None => {}, // we're running below target fps
            Some(sleep_millis) => {
                ::std::thread::sleep(sleep_millis);
            }
        }
    } // 'game_loop

    Ok(())
}
