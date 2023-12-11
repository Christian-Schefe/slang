use std::collections::HashMap;

use crate::{
    builtin_functions::{exec_builtin, is_builtin},
    *,
};

#[derive(Debug)]
pub enum Command {
    Return(VariableValue),
    Break(VariableValue),
    Continue,
    Error(RuntimeError),
}

pub fn execute_program(
    program: String,
) -> Result<(VariableValue, Vec<HashMap<String, VariableValue>>), Error> {
    let tokens = tokenize(&program)?;
    let reduced = reduce_brackets_and_parenths(&tokens)?;

    let statements = get_statements(&reduced)?;
    let mut scope = vec![HashMap::new()];
    let r = match exec_stmnts(&mut scope, &statements) {
        Ok(v) => Ok(v.unwrap_or(VariableValue::Unit)),
        Err(Command::Error(e)) => Err(e.into()),
        Err(Command::Return(v)) => Ok(v),
        Err(cmd) => Err(RuntimeError(format!("Command {:?} cannot leave module", cmd)).into()),
    };
    println!("{:?}", scope);
    r.map(|v| (v, scope))
}

pub fn exec_stmnts(
    scope: &mut Scope,
    stmnts: &[Statement],
) -> Result<Option<VariableValue>, Command> {
    for stmnt in stmnts {
        if let Some(return_val) = exec_stmnt(scope, stmnt)? {
            return Ok(Some(return_val));
        }
    }
    Ok(None)
}

pub fn exec_stmnt(scope: &mut Scope, stmnt: &Statement) -> Result<Option<VariableValue>, Command> {
    info!("exec: {:?}", stmnt);
    match stmnt {
        Statement::VariableDefinition(var, val) => define_var(scope, var, val).map(|_| None),
        Statement::VariableAssignment(var, val) => assign_var(scope, var, val).map(|_| None),
        Statement::Expr(expr) => eval_expr(scope, expr).map(|_| None),
        Statement::Return(expr) => Err(Command::Return(eval_expr(scope, expr)?)),
        Statement::Break(expr) => Err(Command::Break(eval_expr(scope, expr)?)),
        Statement::Continue => Err(Command::Continue),
        Statement::ImplicitReturn(expr) => {
            eval_expr(scope, expr).map(|return_val| Some(return_val))
        }
    }
}

pub fn eval_expr(scope: &mut Scope, expr: &Expression) -> Result<VariableValue, Command> {
    match expr {
        Expression::Block(stmnts) => {
            enter_scope(scope);
            let result = exec_stmnts(scope, stmnts).map(|v| v.unwrap_or(VariableValue::Unit));
            exit_scope(scope);
            result
        }

        Expression::List(list) => Ok(VariableValue::List(
            list.iter()
                .map(|v| eval_expr(scope, v))
                .collect::<Result<Vec<VariableValue>, Command>>()?,
        )),
        Expression::Object(fields) => Ok(VariableValue::Object(
            fields
                .iter()
                .map(|(key, v)| eval_expr(scope, v).map(|r| (key.clone(), r)))
                .collect::<Result<HashMap<String, VariableValue>, Command>>()?,
        )),
        Expression::Value(var) => Ok(var.clone()),
        Expression::Reference(ref_expr) => get_var_cloned(scope, ref_expr),
        Expression::ForLoop(var_name, iterator, body) => {
            let iter = eval_expr(scope, iterator)?; //TODO: don't accept commands!
            match iter {
                VariableValue::List(list) => {
                    define_var_by_val(scope, &var_name, VariableValue::Unit)?;
                    let mut result = VariableValue::Unit;
                    for val in list {
                        assign_var_by_name(scope, &var_name, val)?;
                        match eval_expr(scope, body) {
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
                    define_var_by_val(scope, &var_name, VariableValue::Unit)?;
                    let mut result = VariableValue::Unit;
                    for (key, _) in object {
                        assign_var_by_name(scope, &var_name, VariableValue::String(key))?;
                        match eval_expr(scope, body) {
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
            if let VariableValue::Boolean(condition) = eval_expr(scope, condition_expr)? {
                if !condition {
                    break Ok(VariableValue::Unit);
                }
                match eval_expr(scope, body) {
                    Ok(_) => (),
                    Err(cmd) => match cmd {
                        Command::Break(val) => break Ok(val),
                        Command::Continue => continue,
                        Command::Return(v) => return Err(Command::Return(v)),
                        Command::Error(e) => return Err(Command::Error(e)),
                    },
                }
            }
        },
        Expression::FunctionCall(func_expr, params) => {
            let p = params
                .iter()
                .map(|v| eval_expr(scope, v))
                .collect::<Result<Vec<VariableValue>, Command>>()?;
            eval_expr(scope, func_expr)?.call(scope, p)
        }
        Expression::BuiltinFunctionCall(name, target, params) => {
            exec_builtin(scope, name, target, params)
        }
        Expression::BinaryOperator(a, b, op) => {
            evaluate_binary_op(eval_expr(scope, a)?, eval_expr(scope, b)?, *op)
                .map_err(|v| Command::Error(v))
        }
        Expression::UnaryOperator(a, op) => evaluate_unary_op(eval_expr(scope, a)?, *op),
        Expression::IfElse(cond_expr, if_expr, else_expr) => {
            if let VariableValue::Boolean(cond) = eval_expr(scope, cond_expr)? {
                if cond {
                    eval_expr(scope, if_expr)
                } else {
                    if let Some(else_e) = else_expr {
                        eval_expr(scope, else_e)
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
    scope: &mut Scope,
    var: &str,
    expr: &Expression,
) -> Result<VariableValue, Command> {
    let val = eval_expr(scope, expr)?;
    define_var_by_val(scope, var, val)
}

pub fn define_var_by_val(
    scope: &mut Scope,
    var: &str,
    val: VariableValue,
) -> Result<VariableValue, Command> {
    define_var_in_scope(scope, var, val).map(|_| VariableValue::Unit)
}

pub fn assign_var_by_name(
    scope: &mut Scope,
    var: &str,
    val: VariableValue,
) -> Result<VariableValue, Command> {
    assign_var_in_scope(scope, var, val).map(|_| VariableValue::Unit)
}

pub fn get_var<'a>(
    scope: &'a mut Scope,
    var_expr: &ReferenceExpr,
) -> Result<&'a mut VariableValue, Command> {
    match var_expr {
        ReferenceExpr::Variable(ref var) => get_var_from_scope(scope, var),
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(scope, index_expr)?;
            if let Expression::Reference(ref_expr) = list_expr {
                let li = get_var(scope, ref_expr)?;
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
                let object = get_var(scope, ref_expr)?;
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
    scope: &mut Scope,
    var_expr: &ReferenceExpr,
) -> Result<VariableValue, Command> {
    match var_expr {
        ReferenceExpr::Variable(ref var) => {
            get_var_from_scope_cloned(scope, var).or_else(|e| is_builtin(var, None).ok_or(e))
        }
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(scope, index_expr)?;
            let li = if let Expression::Reference(ref_expr) = list_expr {
                get_var_cloned(scope, ref_expr)?
            } else {
                eval_expr(scope, list_expr)?
            };
            match (li, index) {
                (VariableValue::List(li_vec), VariableValue::Number(i)) => li_vec
                    .get(i as usize)
                    .cloned()
                    .ok_or(Command::Error("Index out of bounds".into())),
                (VariableValue::String(stri), VariableValue::Number(i)) => stri
                    .chars()
                    .nth(i as usize)
                    .map(|v| VariableValue::String(v.to_string()))
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
                get_var_cloned(scope, ref_expr)?
            } else {
                eval_expr(scope, object_expr)?
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
                    format!("Variable '{}' is not an object", object)
                }
                .into(),
            ))
        }
    }
}

pub fn assign_var(
    scope: &mut Scope,
    var_expr: &ReferenceExpr,
    expr: &Expression,
) -> Result<VariableValue, Command> {
    let val = eval_expr(scope, expr)?;
    match var_expr {
        ReferenceExpr::Variable(ref var) => assign_var_by_name(scope, var, val),
        ReferenceExpr::Index(list_expr, index_expr) => {
            let index = eval_expr(scope, index_expr)?;
            if let Expression::Reference(ref_expr) = list_expr {
                let list = get_var(scope, ref_expr)?;
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
                let object = get_var(scope, ref_expr)?;
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
