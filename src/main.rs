use std::{
    collections::HashMap,
    env::{self, args},
    fs,
};

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
    let (program, cwd) = read_program_file()?;
    execute_program(program, cwd).map(|_| ())
}

fn read_program_file() -> Result<(String, String), ClientError> {
    let args: Vec<String> = args().collect();
    let path = args
        .get(1)
        .ok_or(ClientError("No argument 'path' was given.".to_owned()))?;
    let program = fs::read_to_string(path)
        .map_err(|e| ClientError(format!("Couldn't read file at {}: {}", path, e)))?;
    let cwd = env::current_dir()
        .map_err(|e| -> ClientError { format!("{}", e).into() })?
        .join(path);
    let cwd_str = cwd
        .parent()
        .ok_or(ClientError("E".into()))?
        .canonicalize()
        .map_err(|e| ClientError(format!("{}", e)))?
        .as_os_str()
        .to_str()
        .unwrap()
        .to_string();
    Ok((program, cwd_str))
}
