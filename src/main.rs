use std::{collections::HashMap, env::args, fs};

use errors::*;
use executor::*;
use log::{error, info};
use parser::*;
use scope::*;
use tokenizer::*;
use variables::*;

mod builtin_functions;
mod errors;
mod executor;
mod parser;
mod scope;
mod tokenizer;
mod variables;

fn main() {
    env_logger::builder()
        .format_module_path(false)
        .format_timestamp(None)
        .format_target(false)
        .init();
    match run() {
        Err(Error::S(e)) => error!("Syntax Error: {}", e.0),
        Err(Error::R(e)) => error!("Runtime Error: {}", e.0),
        Err(Error::C(e)) => error!("Client Error: {}", e.0),
        Ok(()) => (),
    }
}

fn run() -> Result<(), Error> {
    let program = read_program_file()?;
    execute_program(program).map(|_| ())
}

fn read_program_file() -> Result<String, ClientError> {
    let args: Vec<String> = args().collect();
    let path = args
        .get(1)
        .ok_or(ClientError("No argument 'path' was given.".to_owned()))?;
    let program = fs::read_to_string(path)
        .map_err(|e| ClientError(format!("Couldn't read file at {}: {}", path, e)))?;
    Ok(program)
}
