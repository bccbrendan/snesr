extern crate sdl2;

use std::env;
use std::io::prelude::*;
use std::fs::File;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::time::Duration;

fn main() -> std::io::Result<()> {
    println!("Hello, snesr!");
    let args: Vec<String> = env::args().collect();
    let rom_file = &args[1];
    println!("opening {}", rom_file);
    let mut f = File::open(rom_file)?;

    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;
    println!("read {} bytes", buffer.len());

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

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut i = 0;
    'running: loop {
        // handle events from SDL
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                _ => {}
            }
        }
        // handle the reset of the game loop iteration
        i = (i + 1) % 255;
        canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        canvas.clear();
        canvas.present();
        ::std::thread::sleep(Duration::new(0, 1_000_000u32 / 60));
    }
    Ok(())
}
