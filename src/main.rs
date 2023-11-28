use std::{
    env::args,
    fs,
    io::{stdin, stdout, Write},
};

use log::{debug, error, info};
use rand::Rng;
use structs::*;
use tokenizer::*;
use variables::*;

mod structs;
mod tokenizer;
mod variables;

fn main() {
    env_logger::init();
    match read_program_file() {
        Ok(program) => match tokenize(&program) {
            Ok(tokens) => {
                info!("program: {:?}", program);
                let statements = get_statements(tokens);
                match statements {
                    Ok(sta) => {
                        let mut context = Context {
                            cur_layer: 0,
                            layers: vec![Scope::new()],
                        };
                        if let Err(e) = execute_statements(&mut context, sta) {
                            error!("Runtime Error: {}", e.0);
                        }
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
    context: &mut Context,
    statements: Vec<Statement>,
) -> Result<VariableValue, RuntimeError> {
    debug!("Execute {:?}", statements);
    for statement in statements {
        let r = execute_statement(context, statement)?;
        if !matches!(r, VariableValue::Unit) {
            return Ok(r);
        }
    }
    Ok(VariableValue::Unit)
}

fn execute_statement(
    context: &mut Context,
    statement: Statement,
) -> Result<VariableValue, RuntimeError> {
    match statement {
        Statement::Empty => (),
        Statement::VariableDefinition(s, expr) => {
            let val = evaluate_expr(context, expr)?;
            context.define_var(&s, val)?;
        }
        Statement::ReturnStatement(expr) => return evaluate_expr(context, expr),
        Statement::ExpressionStatement(expr) => {
            evaluate_expr(context, expr)?;
        }
        Statement::FunctionDefinition(s, params, expr) => {
            context.define_var(&s, VariableValue::Function(params, Box::new(expr)))?;
        }
        Statement::VariableAssignment(s, expr) => {
            let val = evaluate_expr(context, expr)?;
            context.set_var(&s, val)?;
        }
        Statement::OperatorAssignment(s, expr, op) => {
            let val = evaluate_expr(context, expr)?;
            let var = context.get_var(&s)?;
            let result = evaluate_binary_op(var, val, op)?;
            context.set_var(&s, result)?;
        }
        Statement::WhileLoop(condition, body) => loop {
            let do_iter = evaluate_expr(context, condition.clone())?;
            if let VariableValue::Boolean(true) = do_iter {
                evaluate_expr(context, body.clone())?;
            } else {
                break;
            }
        },
        Statement::ForLoop(statements, condition, body) => {
            let (setup, step) = *statements;
            execute_statement(context, setup)?;
            loop {
                let do_iter = evaluate_expr(context, condition.clone())?;
                if let VariableValue::Boolean(true) = do_iter {
                    evaluate_expr(context, body.clone())?;
                } else {
                    break;
                }
                execute_statement(context, step.clone())?;
            }
        }
    };
    Ok(VariableValue::Unit)
}

fn evaluate_expr(context: &mut Context, expr: Expression) -> Result<VariableValue, RuntimeError> {
    debug!("Evaluate Expr {:?}", expr);
    match expr {
        Expression::Value(x) => Ok(x),
        Expression::List(exprs) => {
            let list = exprs
                .into_iter()
                .map(|expr| evaluate_expr(context, expr))
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            Ok(VariableValue::List(list))
        }
        Expression::Indexing(list_name, index_expr) => {
            let list_val = context.get_var(&list_name)?;
            let index_val = evaluate_expr(context, *index_expr)?;
            match (list_val, index_val) {
                (VariableValue::List(list), VariableValue::Number(i)) => {
                    let index: usize = i
                        .try_into()
                        .map_err(|_| RuntimeError(format!("Invalid index: {}", i)))?;
                    list.get(index)
                        .ok_or(RuntimeError(format!(
                            "Index outside of range: array len is {} but index is {}",
                            list.len(),
                            index
                        )))
                        .cloned()
                }
                (VariableValue::String(list), VariableValue::Number(i)) => {
                    let index: usize = i
                        .try_into()
                        .map_err(|_| RuntimeError(format!("Invalid index: {}", i)))?;
                    list.chars()
                        .nth(index)
                        .map(|c| VariableValue::String(String::from(c)))
                        .ok_or(RuntimeError(format!(
                            "Index outside of range: array len is {} but index is {}",
                            list.len(),
                            index
                        )))
                }
                (a, b) => Err(RuntimeError(format!(
                    "Invalid indexing statement: list of type {}, index {}",
                    a.get_type(),
                    b
                ))),
            }
        }
        Expression::Block(statements) => {
            let mut inner_context = context.create_block_context()?;
            let result = execute_statements(&mut inner_context, statements)?;
            context.apply_block_context(inner_context)?;
            Ok(result)
        }
        Expression::BinaryOperator(l, r, op) => {
            let lval = evaluate_expr(context, *l)?;
            let rval = evaluate_expr(context, *r)?;
            evaluate_binary_op(lval, rval, op)
        }
        Expression::UnaryOperator(expr, op) => {
            let val = evaluate_expr(context, *expr)?;
            evaluate_unary_op(val, op)
        }
        Expression::Reference(var_name) => context.get_var(&var_name),
        Expression::FunctionCall(function_name, params) => {
            let values: Vec<VariableValue> = params
                .into_iter()
                .map(|p| evaluate_expr(context, p))
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            if function_name == "print" {
                for val in values {
                    print!("{} ", val);
                }
                println!();
                Ok(VariableValue::Unit)
            } else if function_name == "random" {
                if let (Some(VariableValue::Number(start)), Some(VariableValue::Number(end))) =
                    (values.get(0), values.get(1))
                {
                    let mut rng = rand::thread_rng();
                    let val = rng.gen_range(*start..*end);
                    Ok(VariableValue::Number(val))
                } else {
                    Err(RuntimeError(format!(
                        "Invalid arguments for 'rand': {:?}",
                        values
                    )))
                }
            } else if function_name == "input" {
                if let Some(val) = values.first() {
                    print!("{}", val);
                }
                stdout()
                    .flush()
                    .map_err(|e| RuntimeError(format!("{}", e)))?;
                let mut input = String::new();
                stdin()
                    .read_line(&mut input)
                    .map_err(|e| RuntimeError(format!("{}", e)))?;
                Ok(VariableValue::String(input.trim_end().to_owned()))
            } else if function_name == "int" {
                if let Some(VariableValue::String(s)) = values.first() {
                    Ok(VariableValue::Number(str::parse(s.trim()).map_err(
                        |_| {
                            RuntimeError(format!(
                                "invalid arguments to function 'int': {:?}",
                                values
                            ))
                        },
                    )?))
                } else {
                    Err(RuntimeError(
                        "invalid arguments to function 'int'".to_owned(),
                    ))
                }
            } else if function_name == "len" {
                if let Some(VariableValue::List(list)) = values.first() {
                    Ok(VariableValue::Number(list.len() as i32))
                } else {
                    Err(RuntimeError(
                        "invalid arguments to function 'len'".to_owned(),
                    ))
                }
            } else if let Some((_, VariableValue::Function(args, body))) =
                context.try_get_var(&function_name)
            {
                let mut inner_context = context.create_fn_context(&function_name)?;
                for i in 0..values.len() {
                    inner_context.define_var(&args[i], values[i].clone())?;
                }
                let result = evaluate_expr(&mut inner_context, *body)?;
                context.apply_fn_context(&function_name, inner_context)?;
                Ok(result)
            } else {
                Err(RuntimeError(format!(
                    "Function '{}' does not exist",
                    function_name
                )))
            }
        }
        Expression::IfElse(condition, if_body, maybe_else_body) => {
            let do_if = evaluate_expr(context, *condition)?;
            if let VariableValue::Boolean(true) = do_if {
                evaluate_expr(context, *if_body)
            } else if let Some(else_body) = maybe_else_body {
                evaluate_expr(context, *else_body)
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
