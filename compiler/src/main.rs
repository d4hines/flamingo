#![allow(non_camel_case_types, non_snake_case)]
use std::env;
use std::fs;
use differential_datalog::ddval;
use differential_datalog::ddval::DDValConvert;
use differential_datalog::program::RelId;
use differential_datalog::program::Update;
use differential_datalog::record::Record;
use differential_datalog::DDlog;
use compile_ddlog::api::HDDlog;
use types::*;
use value::Relations;
use value::Value;

fn print_ddlog() {

}

fn main() {
    let alm_file = env::args()
        .nth(1)
        .expect("Please specify an ALM module to compile.");

    let read_file_error = format!("Could not read file {}!", alm_file);
    let contents = fs::read_to_string(alm_file).expect(read_file_error.as_str());

    println!("{}", contents);
}
