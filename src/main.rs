use std::{collections::HashMap, fs};

use log::{debug, error, info};
use structs::*;
use tokenizer::*;

mod structs;
mod tokenizer;

fn main() {
    env_logger::init();
    if let Ok(program) = fs::read_to_string("program.slang") {
        info!("program: {:?}", program);
        match tokenize(&program) {
            Ok(tokens) => {
                let statements = get_statements(tokens);
                match statements {
                    Ok(sta) => {
                        let mut context = Context {
                            variables: HashMap::new(),
                            functions: HashMap::new(),
                        };
                        if let Err(e) = execute(&mut context, sta) {
                            error!("Error {:?}", e);
                        }

                        info!("{:?}", context);
                    }
                    Err(e) => {
                        error!("couldn't parse program: {:?}", e)
                    }
                }
            }
            Err(e) => error!("couldn't parse program: {:?}", e),
        }
    }
}

fn execute(
    context: &mut Context,
    statements: Vec<Statement>,
) -> Result<VariableValue, RuntimeError> {
    debug!("Execute {:?}", statements);
    for statement in statements {
        match statement {
            Statement::Empty => (),
            Statement::VariableDefinition(s, expr) => {
                let val = evaluate_expr(context, expr)?;
                if context.variables.contains_key(&s) {
                    return Err(RuntimeError(format!("Variable '{}' already exists", s)));
                }
                context.variables.insert(s, val);
            }
            Statement::ReturnStatement(expr) => return evaluate_expr(context, expr),
            Statement::ExpressionStatement(expr) => {
                evaluate_expr(context, expr)?;
            }
            Statement::FunctionDefinition(s, params, expr) => {
                if context.functions.contains_key(&s) {
                    return Err(RuntimeError(format!("Function '{}' already exists", s)));
                }
                context.functions.insert(s, (params, expr));
            }
            Statement::VariableAssignment(s, expr) => {
                let val = evaluate_expr(context, expr)?;
                set_var(context, &s, val)?;
            }
        }
    }
    Ok(VariableValue::Void)
}

fn evaluate_expr(context: &mut Context, expr: Expression) -> Result<VariableValue, RuntimeError> {
    debug!("Evaluate Expr {:?}", expr);
    match expr {
        Expression::Value(x) => Ok(x),
        Expression::Block(statements) => {
            let mut inner_context = Context {
                variables: context.variables.clone(),
                functions: context.functions.clone(),
            };
            execute(&mut inner_context, statements)
        }
        Expression::ComputedValue(l, r, op) => {
            let lval = evaluate_expr(context, *l)?;
            let rval = evaluate_expr(context, *r)?;
            evaluate_op(lval, rval, op)
        }
        Expression::Reference(var_name) => get_var(context, &var_name),
        Expression::FunctionCall(function_name, params) => {
            let values: Vec<VariableValue> = params
                .into_iter()
                .map(|p| evaluate_expr(context, p))
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            if function_name == "print" {
                println!("{:?}", values);
                Ok(VariableValue::Void)
            } else if let Some((args, expr)) = context.functions.get(&function_name) {
                let mut inner_context = Context {
                    variables: HashMap::new(),
                    functions: HashMap::new(),
                };
                for i in 0..values.len() {
                    inner_context
                        .variables
                        .insert(args[i].to_owned(), values[i].clone());
                }
                evaluate_expr(&mut inner_context, expr.clone())
            } else {
                Err(RuntimeError(format!(
                    "Function '{}' does not exist",
                    function_name
                )))
            }
        }
    }
}

fn get_var(context: &Context, var: &String) -> Result<VariableValue, RuntimeError> {
    context
        .variables
        .get(var)
        .cloned()
        .ok_or(RuntimeError(format!("Variable '{}' does not exist", var)))
}

fn set_var(
    context: &mut Context,
    var: &String,
    val: VariableValue,
) -> Result<(), RuntimeError> {
    let v = context
        .variables
        .get_mut(var)
        .ok_or(RuntimeError(format!("Variable '{}' does not exist", var)))?;
    *v = val;
    Ok(())
}

fn evaluate_op(
    a: VariableValue,
    b: VariableValue,
    op: Operator,
) -> Result<VariableValue, RuntimeError> {
    debug!("Evaluate Operator {:?}", op);
    if let VariableValue::Number(na) = a {
        if let VariableValue::Number(nb) = b {
            match op {
                Operator::Add => Ok(VariableValue::Number(na + nb)),
                Operator::Subtract => Ok(VariableValue::Number(na - nb)),
                Operator::Multiply => Ok(VariableValue::Number(na * nb)),
            }
        } else {
            Err(RuntimeError(format!("Not implemented")))
        }
    } else {
        Err(RuntimeError(format!("Not implemented")))
    }
}
