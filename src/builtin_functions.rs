use std::{collections::HashMap, fs};

use crate::{executor::Command, parser::Expression, variables::VariableValue, scope::Scope};

pub fn exec_builtin(
    scope: &mut Scope,
    name: &str,
    target: &Option<VariableValue>,
    params: &Vec<VariableValue>,
) -> Result<VariableValue, Command> {
    match name {
        "print" => {
            if let Some(print_target) = target {
                println!("{}", print_target)
            } else {
                for (i, val) in params.iter().enumerate() {
                    if i == 0 {
                        print!("{}", val)
                    } else {
                        print!(" {}", val)
                    }
                }
                println!();
            }
            Ok(VariableValue::Unit)
        }
        "int" => {
            if params.len() != 1 {
                Err(Command::Error(
                    "Invalid parameter amount for function 'int'".into(),
                ))
            } else if let Some(VariableValue::String(val)) = params.get(0) {
                str::parse::<i32>(val)
                    .map_err(|_| Command::Error("Cannot parse str to int".into()))
                    .map(|v| VariableValue::Number(v))
            } else {
                Err(Command::Error("Cannot parse to int".into()))
            }
        }
        "read" => {
            if params.len() != 1 {
                Err(Command::Error(
                    "Invalid parameter amount for function 'read'".into(),
                ))
            } else if let Some(VariableValue::String(val)) = params.get(0) {
                fs::read_to_string(val)
                    .map_err(|_| Command::Error("Cannot read file".into()))
                    .map(|v| VariableValue::String(v))
            } else {
                Err(Command::Error("Param is not a string".into()))
            }
        }
        "lines" => {
            if params.len() != 0 {
                Err(Command::Error(
                    "Invalid parameter amount for function 'lines'".into(),
                ))
            } else if let Some(VariableValue::String(val)) = target {
                val.lines()
                    .map(|v| Ok(VariableValue::String(v.trim().to_string())))
                    .collect::<Result<Vec<VariableValue>, Command>>()
                    .map(|l| VariableValue::List(l))
            } else {
                Err(Command::Error("target is not a string".into()))
            }
        }
        "list" => {
            if params.len() != 1 {
                Err(Command::Error(
                    "Invalid parameter amount for function 'list'".into(),
                ))
            } else if let Some(VariableValue::String(val)) = params.get(0) {
                Ok(VariableValue::List(
                    val.chars()
                        .map(|c| VariableValue::String(c.to_string()))
                        .collect(),
                ))
            } else {
                Err(Command::Error("Cannot convert to list".into()))
            }
        }
        "split" => {
            if let (Some(VariableValue::String(split)), Some(VariableValue::String(splitter))) =
                (target, params.get(0))
            {
                Ok(VariableValue::List(
                    split
                        .split(splitter)
                        .map(|s| VariableValue::String(s.to_string()))
                        .collect(),
                ))
            } else {
                Err(Command::Error("Invalid arguments for method split".into()))
            }
        }
        "map" => {
            if let Some(VariableValue::Function(_, _)) = params.get(0) {
                match target {
                    Some(VariableValue::List(li)) => li
                        .iter()
                        .map(|el| params[0].call(scope, vec![el.clone()]))
                        .collect::<Result<Vec<VariableValue>, Command>>()
                        .map(|v| VariableValue::List(v)),
                    Some(VariableValue::Object(li)) => li
                        .iter()
                        .map(|(key, el)| {
                            params[0]
                                .call(
                                    scope,
                                    vec![VariableValue::String(key.to_string()), el.clone()],
                                )
                                .map(|res| (key.to_string(), res))
                        })
                        .collect::<Result<HashMap<String, VariableValue>, Command>>()
                        .map(|v| VariableValue::Object(v)),
                    Some(VariableValue::String(li)) => li
                        .chars()
                        .map(|el| {
                            params[0].call(scope, vec![VariableValue::String(el.to_string())])
                        })
                        .collect::<Result<Vec<VariableValue>, Command>>()
                        .map(|v| VariableValue::List(v)),
                    _ => Err(Command::Error("invalid target for map".into())),
                }
            } else {
                Err(Command::Error("Invalid arguments for method map".into()))
            }
        }
        "len" => match target {
            Some(VariableValue::List(li)) => Ok(VariableValue::Number(li.len() as i32)),
            Some(VariableValue::Object(li)) => Ok(VariableValue::Number(li.len() as i32)),
            Some(VariableValue::String(li)) => Ok(VariableValue::Number(li.len() as i32)),
            _ => Err(Command::Error("invalid target for map".into())),
        },
        "filter" => {
            if let Some(VariableValue::Function(_, _)) = params.get(0) {
                match target {
                    Some(VariableValue::List(li)) => li
                        .iter()
                        .filter_map(|el| match params[0].call(scope, vec![el.clone()]) {
                            Err(e) => Some(Err(e)),
                            Ok(VariableValue::Boolean(b)) if b => Some(Ok(el.clone())),
                            Ok(VariableValue::Boolean(_)) => None,
                            Ok(_) => Some(Err(Command::Error("not a bool".into()))),
                        })
                        .collect::<Result<Vec<VariableValue>, Command>>()
                        .map(|v| VariableValue::List(v)),
                    Some(VariableValue::Object(li)) => li
                        .iter()
                        .filter_map(|(key, el)| {
                            match params[0].call(
                                scope,
                                vec![VariableValue::String(key.to_string()), el.clone()],
                            ) {
                                Err(e) => Some(Err(e)),
                                Ok(VariableValue::Boolean(b)) if b => {
                                    Some(Ok((key.to_string(), el.clone())))
                                }
                                Ok(VariableValue::Boolean(_)) => None,
                                Ok(_) => Some(Err(Command::Error("not a bool".into()))),
                            }
                        })
                        .collect::<Result<HashMap<String, VariableValue>, Command>>()
                        .map(|v| VariableValue::Object(v)),
                    Some(VariableValue::String(li)) => li
                        .chars()
                        .filter_map(|el| {
                            match params[0].call(scope, vec![VariableValue::String(el.to_string())])
                            {
                                Err(e) => Some(Err(e)),
                                Ok(VariableValue::Boolean(b)) if b => {
                                    Some(Ok(VariableValue::String(el.to_string())))
                                }
                                Ok(VariableValue::Boolean(_)) => None,
                                Ok(_) => Some(Err(Command::Error("not a bool".into()))),
                            }
                        })
                        .collect::<Result<Vec<VariableValue>, Command>>()
                        .map(|v| VariableValue::List(v)),
                    _ => Err(Command::Error("invalid target for map".into())),
                }
            } else {
                Err(Command::Error("Invalid arguments for method filter".into()))
            }
        }
        _ => Err(Command::Error("not a builtin function".into())),
    }
}

pub fn is_builtin(name: &str, target: Option<&VariableValue>) -> Option<VariableValue> {
    if match (target, name) {
        (_, "print") => true,
        (_, "list") => true,
        (_, "int") => true,
        (_, "read") => true,
        (_, "lines") => true,
        (Some(VariableValue::String(_)), "split") => true,
        (Some(VariableValue::String(_)), "map") => true,
        (Some(VariableValue::List(_)), "map") => true,
        (Some(VariableValue::Object(_)), "map") => true,
        (Some(VariableValue::String(_)), "filter") => true,
        (Some(VariableValue::List(_)), "filter") => true,
        (Some(VariableValue::Object(_)), "filter") => true,
        (Some(VariableValue::String(_)), "len") => true,
        (Some(VariableValue::List(_)), "len") => true,
        (Some(VariableValue::Object(_)), "len") => true,
        (_, _) => false,
    } {
        Some(VariableValue::Function(
            Vec::new(),
            Box::new(Expression::BuiltinFunctionCall(
                name.to_string(),
                target.cloned(),
                Vec::new(),
            )),
        ))
    } else {
        None
    }
}
