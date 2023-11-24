use std::fs;

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
                        let mut scope = Scope::new();
                        if let Err(e) = execute(&mut scope, sta) {
                            error!("Error {:?}", e);
                        }

                        info!("{:?}", scope);
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

fn execute(scope: &mut Scope, statements: Vec<Statement>) -> Result<VariableValue, RuntimeError> {
    debug!("Execute {:?}", statements);
    for statement in statements {
        match statement {
            Statement::Empty => (),
            Statement::VariableDefinition(s, expr) => {
                let val = evaluate_expr(scope, expr)?;
                scope.define_var(&s, val)?;
            }
            Statement::ReturnStatement(expr) => return evaluate_expr(scope, expr),
            Statement::ExpressionStatement(expr) => {
                evaluate_expr(scope, expr)?;
            }
            Statement::FunctionDefinition(s, params, expr) => {
                scope.define_var(&s, VariableValue::Function(params, Box::new(expr)))?;
            }
            Statement::VariableAssignment(s, expr) => {
                let val = evaluate_expr(scope, expr)?;
                scope.set_var(&s, val)?;
            }
            Statement::WhileLoop(condition, body) => loop {
                let do_iter = evaluate_expr(scope, condition.clone())?;
                if let VariableValue::Boolean(true) = do_iter {
                    evaluate_expr(scope, body.clone())?;
                } else {
                    break;
                }
            },
        }
    }
    Ok(VariableValue::Void)
}

fn evaluate_expr(scope: &mut Scope, expr: Expression) -> Result<VariableValue, RuntimeError> {
    debug!("Evaluate Expr {:?}", expr);
    match expr {
        Expression::Value(x) => Ok(x),
        Expression::Block(statements) => {
            let result = execute(scope, statements)?;
            Ok(result)
        }
        Expression::ComputedValue(l, r, op) => {
            let lval = evaluate_expr(scope, *l)?;
            let rval = evaluate_expr(scope, *r)?;
            evaluate_op(lval, rval, op)
        }
        Expression::Reference(var_name) => scope.get_var(&var_name),
        Expression::FunctionCall(function_name, params) => {
            let values: Vec<VariableValue> = params
                .into_iter()
                .map(|p| evaluate_expr(scope, p))
                .collect::<Result<Vec<VariableValue>, RuntimeError>>(
            )?;
            if function_name == "print" {
                for val in values {
                    println!("{}", val);
                }
                Ok(VariableValue::Void)
            } else if let Some(VariableValue::Function(args, body)) =
                scope.try_get_var(&function_name)
            {
                let mut inner_scope = Scope::new();
                for i in 0..values.len() {
                    inner_scope
                        .variables
                        .insert(args[i].to_owned(), values[i].clone());
                }
                let result = evaluate_expr(&mut inner_scope, *body)?;
                Ok(result)
            } else {
                Err(RuntimeError(format!(
                    "Function '{}' does not exist",
                    function_name
                )))
            }
        }
        Expression::IfElse(condition, if_body, else_body) => {
            let do_if = evaluate_expr(scope, *condition)?;
            if let VariableValue::Boolean(true) = do_if {
                evaluate_expr(scope, *if_body)
            } else {
                evaluate_expr(scope, *else_body)
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
            }
        } else {
            Err(RuntimeError(format!("Not implemented")))
        }
    } else {
        Err(RuntimeError(format!("Not implemented")))
    }
}
