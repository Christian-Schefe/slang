use crate::{context::RuntimeError, execute_function, variables::VariableValue, Context};

pub fn try_get_builtin_function(
    target: Option<VariableValue>,
    function_name: String,
) -> Option<VariableValue> {
    if match (&target, function_name.as_str()) {
        (Some(VariableValue::String(_)), "split") => true,
        (Some(VariableValue::List(_)), "map") => true,
        (None, "print") => true,
        (_, _) => false,
    } {
        Some(VariableValue::BuiltinFunction(
            target.map(|v| Box::new(v)),
            function_name,
        ))
    } else {
        None
    }
}

pub fn execute_builtin_function(
    context: &mut Context,
    target: Option<VariableValue>,
    function_name: String,
    params: Vec<VariableValue>,
    layer: usize,
) -> Result<VariableValue, RuntimeError> {
    match target {
        Some(VariableValue::String(s)) => match function_name.as_str() {
            "split" => string_split(s, params),
            _ => Err(RuntimeError("???".to_string())),
        },
        Some(VariableValue::List(l)) => match function_name.as_str() {
            "map" => list_map(context, l, params, layer),
            _ => Err(RuntimeError("???".to_string())),
        },
        None => match function_name.as_str() {
            "print" => print(params),
            _ => Err(RuntimeError("???".to_string())),
        },
        _ => Err(RuntimeError("???".to_string())),
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
        Some(VariableValue::BuiltinFunction(target2, name)) => {
            let new_list = target
                .into_iter()
                .map(|el| {
                    execute_builtin_function(
                        context,
                        target2.clone().map(|b| *b),
                        name.to_string(),
                        vec![el],
                        layer,
                    )
                })
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            Ok(VariableValue::List(new_list))
        }
        _ => Err(RuntimeError(
            "Incorrect arguments for function 'string.split'".to_string(),
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
