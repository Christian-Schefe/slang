use std::{env::args, fs};

use log::{debug, error, info};
use structs::*;
use tokenizer::*;

mod structs;
mod tokenizer;

fn main() {
    env_logger::init();
    match read_program_file() {
        Ok(program) => match tokenize(&program) {
            Ok(tokens) => {
                info!("program: {:?}", program);
                let statements = get_statements(tokens);
                match statements {
                    Ok(sta) => {
                        let mut scope = Scope::new();
                        if let Err(e) = execute_statements(&mut scope, sta) {
                            error!("Runtime Error: {}", e.0);
                        }

                        info!("{:?}", scope);
                    }
                    Err(e) => {
                        error!("Syntax Error (Statements): {}", e.0)
                    }
                }
            }
            Err(e) => error!("Syntax Error (Tokens): {}", e.0),
        },
        Err(e) => error!("SLANG didn't execute successfully: {}", e.0),
    }
}

fn read_program_file() -> Result<String, ClientError> {
    let args: Vec<String> = args().collect();
    let path = args
        .get(1)
        .ok_or(ClientError("No argument 'path' was given.".to_owned()))?;
    let program = fs::read_to_string(path)
        .map_err(|e| ClientError(format!("Couldn't read file at {}: {}", path, e)))?;
    Ok(program)
}

fn execute_statements(
    scope: &mut Scope,
    statements: Vec<Statement>,
) -> Result<VariableValue, RuntimeError> {
    debug!("Execute {:?}", statements);
    for statement in statements {
        let r = execute_statement(scope, statement)?;
        if !matches!(r, VariableValue::Unit) {
            return Ok(r);
        }
    }
    Ok(VariableValue::Unit)
}

fn execute_statement(
    scope: &mut Scope,
    statement: Statement,
) -> Result<VariableValue, RuntimeError> {
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
        Statement::ForLoop(statements, condition, body) => {
            let (setup, step) = *statements;
            execute_statement(scope, setup)?;
            loop {
                let do_iter = evaluate_expr(scope, condition.clone())?;
                if let VariableValue::Boolean(true) = do_iter {
                    evaluate_expr(scope, body.clone())?;
                } else {
                    break;
                }
                execute_statement(scope, step.clone())?;
            }
        }
    };
    Ok(VariableValue::Unit)
}

fn evaluate_expr(scope: &mut Scope, expr: Expression) -> Result<VariableValue, RuntimeError> {
    debug!("Evaluate Expr {:?}", expr);
    match expr {
        Expression::Value(x) => Ok(x),
        Expression::Block(statements) => {
            let result = execute_statements(scope, statements)?;
            Ok(result)
        }
        Expression::BinaryOperator(l, r, op) => {
            let lval = evaluate_expr(scope, *l)?;
            let rval = evaluate_expr(scope, *r)?;
            evaluate_binary_op(lval, rval, op)
        }
        Expression::UnaryOperator(expr, op) => {
            let val = evaluate_expr(scope, *expr)?;
            evaluate_unary_op(val, op)
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
                    print!("{} ", val);
                }
                println!();
                Ok(VariableValue::Unit)
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
        Expression::IfElse(condition, if_body, maybe_else_body) => {
            let do_if = evaluate_expr(scope, *condition)?;
            if let VariableValue::Boolean(true) = do_if {
                evaluate_expr(scope, *if_body)
            } else if let Some(else_body) = maybe_else_body {
                evaluate_expr(scope, *else_body)
            } else {
                Ok(VariableValue::Unit)
            }
        }
    }
}

fn evaluate_binary_op(
    a: VariableValue,
    b: VariableValue,
    op: Operator,
) -> Result<VariableValue, RuntimeError> {
    match op {
        Operator::Add => VariableValue::add(a, b),
        Operator::Subtract => VariableValue::subtract(a, b),
        Operator::Multiply => VariableValue::multiply(a, b),
        Operator::Equal => VariableValue::equals(a, b),
        Operator::NotEqual => VariableValue::not_equals(a, b),
        Operator::LessThan => VariableValue::less_than(a, b),
        Operator::LessThanOrEqual => VariableValue::less_than_or_equal(a, b),
        Operator::GreaterThan => VariableValue::greater_than(a, b),
        Operator::GreaterThanOrEqual => VariableValue::greater_than_or_equal(a, b),
        _ => Err(RuntimeError(format!("{:?} is not a binary operator!", op))),
    }
}

fn evaluate_unary_op(a: VariableValue, op: Operator) -> Result<VariableValue, RuntimeError> {
    match op {
        Operator::Not => VariableValue::not(a),
        Operator::Negate => VariableValue::negate(a),
        Operator::UnaryPlus => VariableValue::unary_plus(a),
        _ => Err(RuntimeError(format!("{:?} is not a unary operator!", op))),
    }
}
