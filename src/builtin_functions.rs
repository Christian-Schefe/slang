use std::fs;

use crate::{
    context::RuntimeError, execute_function, expressions::ReferenceExpr, set_reference,
    variables::VariableValue, Context,
};

pub fn try_get_builtin_function(
    target: Option<VariableValue>,
    target_ref: Option<ReferenceExpr>,
    function_name: String,
) -> Option<VariableValue> {
    if match (&target, function_name.as_str()) {
        (Some(VariableValue::String(_)), "split") => true,
        (Some(VariableValue::String(_)), "len") => true,
        (Some(VariableValue::String(_)), "trim") => true,
        (Some(VariableValue::String(_)), "lines") => true,
        (Some(VariableValue::List(_)), "map") => true,
        (Some(VariableValue::List(_)), "filter") => true,
        (Some(VariableValue::List(_)), "len") => true,
        (Some(VariableValue::List(_)), "push") => true,
        (Some(VariableValue::List(_)), "concat") => true,
        (None, "print") => true,
        (None, "typeof") => true,
        (None, "read_file") => true,
        (_, _) => false,
    } {
        Some(VariableValue::BuiltinFunction(
            target.map(|v| Box::new(v)),
            target_ref,
            function_name,
        ))
    } else {
        None
    }
}

pub fn execute_builtin_function(
    context: &mut Context,
    target: Option<VariableValue>,
    target_ref: Option<ReferenceExpr>,
    function_name: String,
    params: Vec<VariableValue>,
    layer: usize,
) -> Result<VariableValue, RuntimeError> {
    match target {
        Some(VariableValue::String(s)) => match function_name.as_str() {
            "len" => Ok(VariableValue::Number(s.len() as i32)),
            "split" => string_split(s, params),
            "trim" => Ok(VariableValue::String(s.trim().to_string())),
            "lines" => Ok(VariableValue::List(
                s.lines()
                    .map(|v| VariableValue::String(v.to_string()))
                    .collect(),
            )),
            _ => Err(RuntimeError("???".to_string())),
        },
        Some(VariableValue::List(l)) => match function_name.as_str() {
            "len" => Ok(VariableValue::Number(l.len() as i32)),
            "map" => list_map(context, l, params, layer),
            "filter" => list_filter(context, l, params, layer),
            "concat" => list_concat(l, params),
            "push" => list_push(context, l, params, target_ref),
            _ => Err(RuntimeError("???".to_string())),
        },
        None => match function_name.as_str() {
            "print" => print(params),
            "typeof" => print_type(params),
            "read_file" => read_file(params),
            _ => Err(RuntimeError("???".to_string())),
        },
        _ => Err(RuntimeError("???".to_string())),
    }
}

fn read_file(params: Vec<VariableValue>) -> Result<VariableValue, RuntimeError> {
    let var = params.get(0).ok_or(RuntimeError("???".to_string()))?;
    if let VariableValue::String(file_path) = var {
        let val = fs::read_to_string(file_path).map_err(|e| RuntimeError(e.to_string()))?;
        Ok(VariableValue::String(val))
    } else {
        Err(RuntimeError("File Path must be string".to_string()))
    }
}

fn print_type(params: Vec<VariableValue>) -> Result<VariableValue, RuntimeError> {
    let val = params.get(0).ok_or(RuntimeError("???".to_string()))?;
    println!("{}", val.get_type());
    Ok(VariableValue::Unit)
}

fn list_push(
    context: &mut Context,
    mut target: Vec<VariableValue>,
    params: Vec<VariableValue>,
    target_ref: Option<ReferenceExpr>,
) -> Result<VariableValue, RuntimeError> {
    match params.get(0) {
        Some(v) => {
            target.push(v.clone());
            set_reference(context, target_ref.unwrap(), VariableValue::List(target))?;
            Ok(VariableValue::Unit)
        }
        _ => Err(RuntimeError(
            "Incorrect arguments for function 'list.push'".to_string(),
        )),
    }
}

fn list_concat(
    target: Vec<VariableValue>,
    params: Vec<VariableValue>,
) -> Result<VariableValue, RuntimeError> {
    match params.get(0) {
        Some(VariableValue::List(l)) => {
            let new_list = target.into_iter().chain(l.clone().into_iter()).collect();
            Ok(VariableValue::List(new_list))
        }
        _ => Err(RuntimeError(
            "Incorrect arguments for function 'list.concat'".to_string(),
        )),
    }
}

fn list_map(
    context: &mut Context,
    target: Vec<VariableValue>,
    params: Vec<VariableValue>,
    layer: usize,
) -> Result<VariableValue, RuntimeError> {
    match params.get(0) {
        Some(VariableValue::Function(args, body)) => {
            let new_list = target
                .into_iter()
                .map(|el| execute_function(context, args.clone(), *body.clone(), vec![el], layer))
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            Ok(VariableValue::List(new_list))
        }
        Some(VariableValue::BuiltinFunction(target2, target_ref2, name)) => {
            let new_list = target
                .into_iter()
                .map(|el| {
                    execute_builtin_function(
                        context,
                        target2.clone().map(|b| *b),
                        target_ref2.clone(),
                        name.to_string(),
                        vec![el],
                        layer,
                    )
                })
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            Ok(VariableValue::List(new_list))
        }
        _ => Err(RuntimeError(
            "Incorrect arguments for function 'list.map'".to_string(),
        )),
    }
}

fn list_filter(
    context: &mut Context,
    target: Vec<VariableValue>,
    params: Vec<VariableValue>,
    layer: usize,
) -> Result<VariableValue, RuntimeError> {
    match params.get(0) {
        Some(VariableValue::Function(args, body)) => {
            let new_list = target
                .into_iter()
                .filter_map(|el| {
                    execute_function(
                        context,
                        args.clone(),
                        *body.clone(),
                        vec![el.clone()],
                        layer,
                    )
                    .map(|r| {
                        if let VariableValue::Boolean(true) = r {
                            Some(Ok(el))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|e| Some(Err(e)))
                })
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            Ok(VariableValue::List(new_list))
        }
        Some(VariableValue::BuiltinFunction(target2, target_ref2, name)) => {
            let new_list = target
                .into_iter()
                .filter_map(|el| {
                    execute_builtin_function(
                        context,
                        target2.clone().map(|b| *b),
                        target_ref2.clone(),
                        name.to_string(),
                        vec![el.clone()],
                        layer,
                    )
                    .map(|r| {
                        if let VariableValue::Boolean(true) = r {
                            Some(Ok(el))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|e| Some(Err(e)))
                })
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            Ok(VariableValue::List(new_list))
        }
        _ => Err(RuntimeError(
            "Incorrect arguments for function 'list.filter'".to_string(),
        )),
    }
}

fn string_split(target: String, params: Vec<VariableValue>) -> Result<VariableValue, RuntimeError> {
    if let Some(VariableValue::String(splitter)) = params.get(0) {
        let split = target
            .split(splitter)
            .map(|s| VariableValue::String(s.to_string()))
            .collect();
        Ok(VariableValue::List(split))
    } else {
        Err(RuntimeError(
            "Incorrect arguments for function 'string.split'".to_string(),
        ))
    }
}

fn print(params: Vec<VariableValue>) -> Result<VariableValue, RuntimeError> {
    for i in 0..params.len() {
        if i == 0 {
            print!("{}", params[i])
        } else {
            print!(" {}", params[i])
        }
    }
    println!();
    Ok(VariableValue::Unit)
}
