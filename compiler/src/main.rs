use std::env;
use std::fs;

mod ast;
mod print;

fn main() {
    let alm_file = env::args()
        .nth(1)
        .expect("Please specify an ALM module to compile.");

    let read_file_error = format!("Could not read file {}!", alm_file);
    let contents = fs::read_to_string(alm_file).expect(read_file_error.as_str());

    println!("{}", contents);
}
