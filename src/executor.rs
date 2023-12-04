use std::collections::HashMap;

use crate::*;

pub enum Command {
    Return(VariableValue),
    Break,
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
                    for val in list {
                        assign_var_by_name(variables, &var_name, val)?;
                        match eval_expr(variables, body) {
                            Ok(_) => (),
                            Err(cmd) => match cmd {
                                Command::Break => break,
                                Command::Continue => continue,
                                Command::Return(v) => return Err(Command::Return(v)),
                                Command::Error(e) => return Err(Command::Error(e)),
                            },
                        }
                    }
                    Ok(VariableValue::Unit)
                }
                VariableValue::Object(object) => {
                    define_var_by_val(variables, &var_name, VariableValue::Unit)?;
                    for (key, _) in object {
                        assign_var_by_name(variables, &var_name, VariableValue::String(key))?;
                        match eval_expr(variables, body) {
                            Ok(_) => (),
                            Err(cmd) => match cmd {
                                Command::Break => break,
                                Command::Continue => continue,
                                Command::Return(v) => return Err(Command::Return(v)),
                                Command::Error(e) => return Err(Command::Error(e)),
                            },
                        }
                    }
                    Ok(VariableValue::Unit)
                }

                _ => Err(Command::Error(
                    format!("cannot iterate over {}", iter).into(),
                )),
            }
        }
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
            _ => Err(Command::Error("not a builtin function".into())),
        },
        Expression::BinaryOperator(a, b, op) => {
            evaluate_binary_op(eval_expr(variables, a)?, eval_expr(variables, b)?, *op)
                .map_err(|v| Command::Error(v))
        }
        Expression::UnaryOperator(a, op) => evaluate_unary_op(eval_expr(variables, a)?, *op),
        Expression::IfElse(cond_expr, if_expr, else_expr) => {
            if let VariableValue::Boolean(cond) = eval_expr(variables, cond_expr)? {
                eval_expr(variables, if cond { if_expr } else { else_expr })
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
        ReferenceExpr::Variable(ref var) => {
            if let Some(val) = variables.get_mut(var) {
                Ok(val)
            } else {
                Err(Command::Error(
                    format!("Variable '{}' is not defined", var).into(),
                ))
            }
        }
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(variables, index_expr)?;
            if let Expression::Reference(ref_expr) = list_expr {
                let li = get_var(variables, ref_expr)?;
                match (li, index) {
                    (VariableValue::List(li_vec), VariableValue::Number(i)) => {
                        if let Some(val) = li_vec.get_mut(i as usize) {
                            Ok(val)
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
                    (VariableValue::Object(obj_map), VariableValue::String(key)) => {
                        if let Some(val) = obj_map.get_mut(&key) {
                            Ok(val)
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
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
                    if let Some(val) = obj.get_mut(index_expr) {
                        Ok(val)
                    } else {
                        Err(Command::Error("Not a field of the object".into()))
                    }
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
        ReferenceExpr::Variable(ref var) => {
            if let Some(val) = variables.get(var) {
                Ok(val.clone())
            } else {
                match var.as_str() {
                    "print" => Ok(VariableValue::Function(
                        Vec::new(),
                        Box::new(Expression::BuiltinFunctionCall(
                            "print".to_string(),
                            None,
                            Vec::new(),
                        )),
                    )),
                    _ => Err(Command::Error(
                        format!("Variable '{}' is not defined", var).into(),
                    )),
                }
            }
        }
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(variables, index_expr)?;
            if let Expression::Reference(ref_expr) = list_expr {
                let li = get_var_cloned(variables, ref_expr)?;
                match (li, index) {
                    (VariableValue::List(li_vec), VariableValue::Number(i)) => {
                        if let Some(val) = li_vec.get(i as usize) {
                            Ok(val.clone())
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
                    (VariableValue::Object(obj_map), VariableValue::String(key)) => {
                        if let Some(val) = obj_map.get(&key) {
                            Ok(val.clone())
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
                    (_, _) => Err(Command::Error("Variable is not a list".into())),
                }
            } else {
                let li = eval_expr(variables, list_expr)?;
                match (li, index) {
                    (VariableValue::List(li_vec), VariableValue::Number(i)) => {
                        if let Some(val) = li_vec.get(i as usize) {
                            Ok(val.clone())
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
                    (VariableValue::Object(obj_map), VariableValue::String(key)) => {
                        if let Some(val) = obj_map.get(&key) {
                            Ok(val.clone())
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
                    (_, _) => Err(Command::Error("Variable is not a list".into())),
                }
            }
        }
        ReferenceExpr::Object(object_expr, index_expr) => {
            if let Expression::Reference(ref_expr) = object_expr {
                let object = get_var_cloned(variables, ref_expr)?;
                if let VariableValue::Object(ref obj) = object {
                    if let Some(val) = obj.get(index_expr) {
                        return Ok(val.clone());
                    }
                }
                match (object, index_expr.as_str()) {
                    (obj, name) if is_builtin(name, Some(&obj)) => Ok(VariableValue::Function(
                        Vec::new(),
                        Box::new(Expression::BuiltinFunctionCall(
                            name.to_string(),
                            Some(obj.clone()),
                            Vec::new(),
                        )),
                    )),
                    (VariableValue::Object(_), field) => Err(Command::Error(
                        format!("Identifier '{}' is not a field of the object", field).into(),
                    )),
                    (obj, _) => Err(Command::Error(
                        format!("Variable '{}' is not an object", obj).into(),
                    )),
                }
            } else {
                let object = eval_expr(variables, object_expr)?;
                if let VariableValue::Object(ref obj) = object {
                    if let Some(val) = obj.get(index_expr) {
                        return Ok(val.clone());
                    }
                }
                match (object, index_expr.as_str()) {
                    (obj, name) if is_builtin(name, Some(&obj)) => Ok(VariableValue::Function(
                        Vec::new(),
                        Box::new(Expression::BuiltinFunctionCall(
                            name.to_string(),
                            Some(obj.clone()),
                            Vec::new(),
                        )),
                    )),
                    (VariableValue::Object(_), field) => Err(Command::Error(
                        format!("Identifier '{}' is not a field of the object", field).into(),
                    )),
                    (obj, _) => Err(Command::Error(
                        format!("Variable '{}' is not an object", obj).into(),
                    )),
                }
            }
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
        ReferenceExpr::Variable(ref var) => {
            if !variables.contains_key(var) {
                Err(Command::Error("Variable is not defined".into()))
            } else {
                variables.insert(var.to_string(), val);
                Ok(VariableValue::Unit)
            }
        }
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(variables, index_expr)?;
            if let Expression::Reference(ref_expr) = list_expr {
                let list = get_var(variables, ref_expr)?;
                match (list, index) {
                    (VariableValue::List(li_vec), VariableValue::Number(i)) => {
                        if let Some(mut_index) = li_vec.get_mut(i as usize) {
                            *mut_index = val;
                            Ok(VariableValue::Unit)
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
                    (VariableValue::Object(obj_map), VariableValue::String(key)) => {
                        if let Some(mut_index) = obj_map.get_mut(&key) {
                            *mut_index = val;
                            Ok(VariableValue::Unit)
                        } else {
                            Err(Command::Error("Index is out of bounds".into()))
                        }
                    }
                    (_, _) => Err(Command::Error("Variable is not a list".into())),
                }
            } else {
                Err(Command::Error("Variable is not reference".into()))
            }
        }
        ReferenceExpr::Object(object_expr, index_expr) => {
            if let Expression::Reference(ref_expr) = object_expr {
                let object = get_var(variables, ref_expr)?;
                if let VariableValue::Object(ref mut scope) = object {
                    if let Some(mut_index) = scope.get_mut(index_expr) {
                        *mut_index = val;
                        Ok(VariableValue::Unit)
                    } else {
                        Err(Command::Error("Not a field of the object".into()))
                    }
                } else {
                    Err(Command::Error("Variable is not an object".into()))
                }
            } else {
                Err(Command::Error("Variable is not a reference".into()))
            }
        }
    }
}
