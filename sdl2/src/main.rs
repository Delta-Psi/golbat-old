extern crate golbat;
extern crate sdl2;

use std::env;
use std::fs::File;
use std::io::Read;

use golbat::Rom;

use sdl2::{event::Event, pixels::Color};

const _PALETTE: &'static [(u8, u8, u8)] = &[
    (144, 188, 15),
    (139, 172, 15),
    (48, 98, 48),
    (15, 56, 15),
];

fn main() {
    let rom_path = env::args().nth(1).unwrap();
    let mut rom_file = File::open(rom_path).unwrap();
    let mut rom_data = Vec::<u8>::new();
    rom_file.read_to_end(&mut rom_data).unwrap();
    let rom = Rom::read(&rom_data).unwrap();
    println!("{:?}", rom.header);

    let sdl_context = sdl2::init().unwrap();
    let sdl_video = sdl_context.video().unwrap();

    let window = sdl_video
        .window("golbat", 160, 144)
        .build()
        .unwrap();

    let mut canvas = window.into_canvas()
        .target_texture()
        .present_vsync()
        .build()
        .unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();
    let mut event_pump = sdl_context.event_pump().unwrap();
    'main_loop: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit{..} => break 'main_loop,
                _ => (),
            }
        }

        canvas.clear();
        canvas.present();
    }
}
