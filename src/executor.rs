use std::collections::HashMap;

use crate::*;

pub enum Command {
    Return(VariableValue),
    Break(VariableValue),
    Continue,
    Error(RuntimeError),
}

pub fn exec_stmnts(
    variables: &mut HashMap<String, VariableValue>,
    stmnts: &[Statement],
) -> Result<Option<VariableValue>, Command> {
    for stmnt in stmnts {
        if let Some(return_val) = exec_stmnt(variables, stmnt)? {
            return Ok(Some(return_val));
        }
    }
    Ok(None)
}

pub fn exec_stmnt(
    variables: &mut HashMap<String, VariableValue>,
    stmnt: &Statement,
) -> Result<Option<VariableValue>, Command> {
    info!("exec: {:?}", stmnt);
    match stmnt {
        Statement::VariableDefinition(var, val) => define_var(variables, var, val).map(|_| None),
        Statement::VariableAssignment(var, val) => assign_var(variables, var, val).map(|_| None),
        Statement::Expr(expr) => eval_expr(variables, expr).map(|_| None),
        Statement::Return(expr) => Err(Command::Return(eval_expr(variables, expr)?)),
        Statement::Break(expr) => Err(Command::Break(eval_expr(variables, expr)?)),
        Statement::Continue => Err(Command::Continue),
        Statement::ImplicitReturn(expr) => {
            eval_expr(variables, expr).map(|return_val| Some(return_val))
        }
    }
}

pub fn eval_expr(
    variables: &mut HashMap<String, VariableValue>,
    expr: &Expression,
) -> Result<VariableValue, Command> {
    match expr {
        Expression::Block(stmnts) => {
            exec_stmnts(variables, stmnts).map(|v| v.unwrap_or(VariableValue::Unit))
        }

        Expression::List(list) => Ok(VariableValue::List(
            list.iter()
                .map(|v| eval_expr(variables, v))
                .collect::<Result<Vec<VariableValue>, Command>>()?,
        )),
        Expression::Object(fields) => Ok(VariableValue::Object(
            fields
                .iter()
                .map(|(key, v)| eval_expr(variables, v).map(|r| (key.clone(), r)))
                .collect::<Result<HashMap<String, VariableValue>, Command>>()?,
        )),
        Expression::Value(var) => Ok(var.clone()),
        Expression::Reference(ref_expr) => get_var_cloned(variables, ref_expr),
        Expression::ForLoop(var_name, iterator, body) => {
            let iter = eval_expr(variables, iterator)?; //TODO: don't accept commands!
            match iter {
                VariableValue::List(list) => {
                    define_var_by_val(variables, &var_name, VariableValue::Unit)?;
                    let mut result = VariableValue::Unit;
                    for val in list {
                        assign_var_by_name(variables, &var_name, val)?;
                        match eval_expr(variables, body) {
                            Ok(_) => (),
                            Err(cmd) => match cmd {
                                Command::Break(v) => {
                                    result = v;
                                    break;
                                }
                                Command::Continue => continue,
                                Command::Return(v) => return Err(Command::Return(v)),
                                Command::Error(e) => return Err(Command::Error(e)),
                            },
                        }
                    }
                    Ok(result)
                }
                VariableValue::Object(object) => {
                    define_var_by_val(variables, &var_name, VariableValue::Unit)?;
                    let mut result = VariableValue::Unit;
                    for (key, _) in object {
                        assign_var_by_name(variables, &var_name, VariableValue::String(key))?;
                        match eval_expr(variables, body) {
                            Ok(_) => (),
                            Err(cmd) => match cmd {
                                Command::Break(v) => {
                                    result = v;
                                    break;
                                }
                                Command::Continue => continue,
                                Command::Return(v) => return Err(Command::Return(v)),
                                Command::Error(e) => return Err(Command::Error(e)),
                            },
                        }
                    }
                    Ok(result)
                }

                _ => Err(Command::Error(
                    format!("cannot iterate over {}", iter).into(),
                )),
            }
        }
        Expression::WhileLoop(condition_expr, body) => loop {
            if let VariableValue::Boolean(condition) = eval_expr(variables, condition_expr)? {
                match eval_expr(variables, body) {
                    Ok(_) => (),
                    Err(cmd) => match cmd {
                        Command::Break(val) => break Ok(val),
                        Command::Continue => continue,
                        Command::Return(v) => return Err(Command::Return(v)),
                        Command::Error(e) => return Err(Command::Error(e)),
                    },
                }
                if !condition {
                    break Ok(VariableValue::Unit);
                }
            }
        },
        Expression::FunctionCall(func_expr, params) => eval_expr(variables, func_expr)?.call(
            params
                .iter()
                .map(|v| eval_expr(variables, v))
                .collect::<Result<Vec<VariableValue>, Command>>()?,
        ),
        Expression::BuiltinFunctionCall(name, target, params) => match name.as_str() {
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
            _ => Err(Command::Error("not a builtin function".into())),
        },
        Expression::BinaryOperator(a, b, op) => {
            evaluate_binary_op(eval_expr(variables, a)?, eval_expr(variables, b)?, *op)
                .map_err(|v| Command::Error(v))
        }
        Expression::UnaryOperator(a, op) => evaluate_unary_op(eval_expr(variables, a)?, *op),
        Expression::IfElse(cond_expr, if_expr, else_expr) => {
            if let VariableValue::Boolean(cond) = eval_expr(variables, cond_expr)? {
                if cond {
                    eval_expr(variables, if_expr)
                } else {
                    if let Some(else_e) = else_expr {
                        eval_expr(variables, else_e)
                    } else {
                        Ok(VariableValue::Unit)
                    }
                }
            } else {
                Err(Command::Error("condition is not a boolean".into()))
            }
        }
    }
}

pub fn define_var(
    variables: &mut HashMap<String, VariableValue>,
    var: &str,
    expr: &Expression,
) -> Result<VariableValue, Command> {
    let val = eval_expr(variables, expr)?;
    define_var_by_val(variables, var, val)
}

pub fn define_var_by_val(
    variables: &mut HashMap<String, VariableValue>,
    var: &str,
    val: VariableValue,
) -> Result<VariableValue, Command> {
    if variables.contains_key(var) {
        Err(Command::Error("Variable is already defined".into()))
    } else {
        variables.insert(var.to_string(), val);
        Ok(VariableValue::Unit)
    }
}

pub fn assign_var_by_name(
    variables: &mut HashMap<String, VariableValue>,
    var: &str,
    val: VariableValue,
) -> Result<VariableValue, Command> {
    if !variables.contains_key(var) {
        Err(Command::Error("Variable is not defined".into()))
    } else {
        variables.insert(var.to_string(), val);
        Ok(VariableValue::Unit)
    }
}

pub fn get_var<'a>(
    variables: &'a mut HashMap<String, VariableValue>,
    var_expr: &ReferenceExpr,
) -> Result<&'a mut VariableValue, Command> {
    match var_expr {
        ReferenceExpr::Variable(ref var) => variables.get_mut(var).ok_or(Command::Error(
            format!("Variable '{}' is not defined", var).into(),
        )),
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(variables, index_expr)?;
            if let Expression::Reference(ref_expr) = list_expr {
                let li = get_var(variables, ref_expr)?;
                match (li, index) {
                    (VariableValue::List(li_vec), VariableValue::Number(i)) => li_vec
                        .get_mut(i as usize)
                        .ok_or(Command::Error("Index is out of bounds".into())),
                    (VariableValue::Object(obj_map), VariableValue::String(key)) => obj_map
                        .get_mut(&key)
                        .ok_or(Command::Error("Index is out of bounds".into())),
                    (_, _) => Err(Command::Error("Variable is not a list".into())),
                }
            } else {
                Err(Command::Error("Variable is not a ref".into()))
            }
        }
        ReferenceExpr::Object(object_expr, index_expr) => {
            if let Expression::Reference(ref_expr) = object_expr {
                let object = get_var(variables, ref_expr)?;
                if let VariableValue::Object(ref mut obj) = object {
                    obj.get_mut(index_expr)
                        .ok_or(Command::Error("Not a field of the object".into()))
                } else {
                    Err(Command::Error("Variable is not an object".into()))
                }
            } else {
                Err(Command::Error("Variable is not a ref".into()))
            }
        }
    }
}
pub fn get_var_cloned(
    variables: &mut HashMap<String, VariableValue>,
    var_expr: &ReferenceExpr,
) -> Result<VariableValue, Command> {
    match var_expr {
        ReferenceExpr::Variable(ref var) => variables
            .get(var)
            .cloned()
            .or_else(|| is_builtin(var, None))
            .ok_or(Command::Error(
                format!("Variable {} is not defined", var).into(),
            )),
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(variables, index_expr)?;
            let li = if let Expression::Reference(ref_expr) = list_expr {
                get_var_cloned(variables, ref_expr)?
            } else {
                eval_expr(variables, list_expr)?
            };
            match (li, index) {
                (VariableValue::List(li_vec), VariableValue::Number(i)) => li_vec
                    .get(i as usize)
                    .cloned()
                    .ok_or(Command::Error("Index out of bounds".into())),
                (VariableValue::Object(obj_map), VariableValue::String(key)) => obj_map
                    .get(&key)
                    .cloned()
                    .ok_or(Command::Error("Index out of bounds".into())),
                (a, b) => Err(Command::Error(
                    format!("{} cannot be indexed by {}", a, b).into(),
                )),
            }
        }
        ReferenceExpr::Object(object_expr, index_expr) => {
            let object = if let Expression::Reference(ref_expr) = object_expr {
                get_var_cloned(variables, ref_expr)?
            } else {
                eval_expr(variables, object_expr)?
            };
            if let VariableValue::Object(ref obj) = object {
                if let Some(val) = obj.get(index_expr) {
                    return Ok(val.clone());
                }
            }
            is_builtin(&index_expr, Some(&object)).ok_or(Command::Error(
                if let VariableValue::Object(_) = object {
                    format!("Identifier '{}' is not a field of the object", index_expr)
                } else {
                    format!("Variable '{}' is not an object", index_expr)
                }
                .into(),
            ))
        }
    }
}

pub fn assign_var(
    variables: &mut HashMap<String, VariableValue>,
    var_expr: &ReferenceExpr,
    expr: &Expression,
) -> Result<VariableValue, Command> {
    let val = eval_expr(variables, expr)?;
    match var_expr {
        ReferenceExpr::Variable(ref var) => assign_var_by_name(variables, var, val),
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(variables, index_expr)?;
            if let Expression::Reference(ref_expr) = list_expr {
                let list = get_var(variables, ref_expr)?;
                match (list, index) {
                    (VariableValue::List(li_vec), VariableValue::Number(i)) => li_vec
                        .get_mut(i as usize)
                        .map(|v| *v = val)
                        .ok_or(Command::Error("Index out of bounds".into())),
                    (VariableValue::Object(obj_map), VariableValue::String(key)) => obj_map
                        .get_mut(&key)
                        .map(|v| *v = val)
                        .ok_or(Command::Error("Index out of bounds".into())),
                    (a, b) => Err(Command::Error(
                        format!("{} cannot be indexed by {}", a, b).into(),
                    )),
                }
                .map(|_| VariableValue::Unit)
            } else {
                Err(Command::Error("Variable is not reference".into()))
            }
        }
        ReferenceExpr::Object(object_expr, index_expr) => {
            if let Expression::Reference(ref_expr) = object_expr {
                let object = get_var(variables, ref_expr)?;
                if let VariableValue::Object(ref mut scope) = object {
                    scope
                        .get_mut(index_expr)
                        .map(|v| *v = val)
                        .ok_or(Command::Error("Not a field of the object".into()))
                        .map(|_| VariableValue::Unit)
                } else {
                    Err(Command::Error("Variable is not an object".into()))
                }
            } else {
                Err(Command::Error("Variable is not a reference".into()))
            }
        }
    }
}
