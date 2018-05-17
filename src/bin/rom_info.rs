extern crate golbat;

use std::env::args;
use std::fs::File;
use std::io::Read;

fn main() {
    let filename = args().nth(1).unwrap();
    let mut file = File::open(filename).unwrap();
    let mut data = Vec::<u8>::new();
    file.read_to_end(&mut data).unwrap();

    let rom = golbat::Rom::read(&data).unwrap();
    println!("{:?}", rom.header);
}
