use std::{collections::HashMap, fs};

use log::{debug, error, info};
use tokenizer::*;

mod tokenizer;

#[derive(Debug)]
struct Context {
    variables: HashMap<String, VariableValue>,
}

#[derive(Debug)]
pub struct RuntimeError(String);

#[derive(Debug)]
pub struct SyntaxError(String);

#[derive(Debug, Clone, Copy)]
pub enum VariableValue {
    Number(i32),
    Boolean(bool),
    Void,
}

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
            Statement::VariableAssignment(s, expr) => {
                let val = evaluate_expr(context, expr)?;
                context.variables.insert(s, val);
            }
            Statement::ReturnStatement(expr) => return evaluate_expr(context, expr),
            Statement::ExpressionStatement(expr) => {
                evaluate_expr(context, expr)?;
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
            };
            execute(&mut inner_context, statements)
        }
        Expression::ComputedValue(l, r, op) => {
            let lval = evaluate_expr(context, *l)?;
            let rval = evaluate_expr(context, *r)?;
            evaluate_op(lval, rval, op)
        }
        Expression::Reference(s) => context
            .variables
            .get(&s)
            .copied()
            .ok_or(RuntimeError(format!("Variable {} does not exist", s))),
        Expression::FunctionCall(s, params) => {
            let values: Vec<VariableValue> = params
                .into_iter()
                .map(|p| evaluate_expr(context, p))
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            if s == "print" {
                println!("{:?}", values);
                Ok(VariableValue::Void)
            } else {
                Err(RuntimeError("Not implemented".to_owned()))
            }
        }
    }
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
                Operator::Assign => Err(RuntimeError(format!(
                    "Invalid 'Assign' Operator for Numbers"
                ))),
            }
        } else {
            Err(RuntimeError(format!("Not implemented")))
        }
    } else {
        Err(RuntimeError(format!("Not implemented")))
    }
}
