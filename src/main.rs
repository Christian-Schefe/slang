use std::{collections::HashMap, env::args, fs};

use builtin_functions::*;
use context::*;
use expressions::*;
use log::{debug, error, info};
use statements::*;
use tokenizer::*;
use variables::*;

mod builtin_functions;
mod context;
mod expressions;
mod statements;
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
    debug!("Execute Stmnt {:?}", statement);
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
        Statement::VariableAssignment(var, expr) => {
            let val = evaluate_expr(context, expr)?;
            set_reference(context, var, val)?;
        }
        Statement::OperatorAssignment(s, expr, op) => {
            let val = evaluate_expr(context, expr)?;
            let var = get_reference(context, s.clone())?;
            let result = evaluate_binary_op(var.clone(), val, op)?;
            set_reference(context, s, result)?;
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
        Expression::Object(vars) => {
            let mut variables = HashMap::with_capacity(vars.len());
            for (key, val_expr) in vars.into_iter() {
                let val = evaluate_expr(context, val_expr)?;
                variables.insert(key, val);
            }
            Ok(VariableValue::Object(Scope { variables }))
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
        Expression::Reference(var) => get_reference(context, var),
        Expression::FunctionCall(function_name, params) => {
            let values: Vec<VariableValue> = params
                .into_iter()
                .map(|p| evaluate_expr(context, p))
                .collect::<Result<Vec<VariableValue>, RuntimeError>>()?;
            let (layer, func) = get_reference_and_layer(context, function_name)?;

            match func {
                VariableValue::Function(args, body) => {
                    execute_function(context, args, *body, values, layer)
                }
                VariableValue::BuiltinFunction(target, name) => {
                    execute_builtin_function(context, target.map(|b| *b), name, values, layer)
                }
                _ => Err(RuntimeError(format!("{} is not a function", func))),
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
        Operator::Divide => VariableValue::divide(a, b),
        Operator::Equal => VariableValue::equals(a, b),
        Operator::NotEqual => VariableValue::not_equals(a, b),
        Operator::LessThan => VariableValue::less_than(a, b),
        Operator::LessThanOrEqual => VariableValue::less_than_or_equal(a, b),
        Operator::GreaterThan => VariableValue::greater_than(a, b),
        Operator::GreaterThanOrEqual => VariableValue::greater_than_or_equal(a, b),
        Operator::And => VariableValue::and(a, b),
        Operator::Or => VariableValue::or(a, b),
        Operator::Modulo => VariableValue::modulo(a, b),
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

fn get_reference(
    context: &mut Context,
    expr: ReferenceExpr,
) -> Result<VariableValue, RuntimeError> {
    get_reference_and_layer(context, expr).map(|(_, v)| v)
}

fn get_reference_and_layer(
    context: &mut Context,
    expr: ReferenceExpr,
) -> Result<(usize, VariableValue), RuntimeError> {
    match expr {
        ReferenceExpr::Variable(var) => {
            context
                .get_var(&var)
                .map(|(a, b)| (a, b.clone()))
                .or_else(|_e| {
                    try_get_builtin_function(None, var)
                        .ok_or(RuntimeError("not a global builtin func".to_string()))
                        .map(|v| (0, v))
                })
        }
        ReferenceExpr::Index(list_ref, index_expr) => {
            let i = evaluate_expr(context, *index_expr)?;
            let (layer, l) = get_reference_and_layer(context, *list_ref)?;
            if let VariableValue::List(list) = l {
                match i {
                    VariableValue::Number(num) => list
                        .get(num as usize)
                        .ok_or(RuntimeError("index out of bounds".to_string()))
                        .map(|v| (layer, v.clone())),
                    _ => Err(RuntimeError("invalid index type".to_string())),
                }
            } else {
                Err(RuntimeError("not a list".to_string()))
            }
        }
        ReferenceExpr::Object(obj_ref, var) => {
            let (layer, o) = get_reference_and_layer(context, *obj_ref)?;
            if let VariableValue::Object(mut obj) = o {
                obj.try_get_var(&var)
                    .ok_or(RuntimeError("invalid property".to_string()))
                    .map(|v| (layer, v.clone()))
            } else {
                try_get_builtin_function(Some(o), var)
                    .ok_or(RuntimeError("not an object".to_string()))
                    .map(|v| (layer, v))
            }
        }
    }
}

fn set_reference(
    context: &mut Context,
    expr: ReferenceExpr,
    val: VariableValue,
) -> Result<(), RuntimeError> {
    match expr {
        ReferenceExpr::Variable(var) => context.set_var(&var, val),
        ReferenceExpr::Index(list_ref, index_expr) => {
            let i = evaluate_expr(context, *index_expr)?;
            let l = get_reference(context, *list_ref.clone())?;
            if let VariableValue::List(mut list) = l {
                match i {
                    VariableValue::Number(num) => {
                        let var = list
                            .get_mut(num as usize)
                            .ok_or(RuntimeError("index out of bounds".to_string()))?;
                        *var = val;

                        set_reference(context, *list_ref, VariableValue::List(list))?;
                        Ok(())
                    }
                    _ => Err(RuntimeError("invalid index type".to_string())),
                }
            } else {
                Err(RuntimeError("not a list".to_string()))
            }
        }
        ReferenceExpr::Object(obj_ref, var) => {
            let o = get_reference(context, *obj_ref.clone())?;
            if let VariableValue::Object(mut obj) = o {
                obj.try_get_var(&var)
                    .ok_or(RuntimeError("invalid property".to_string()))
                    .and_then(|v| {
                        *v = val;
                        Ok(())
                    })?;

                set_reference(context, *obj_ref, VariableValue::Object(obj))?;
                Ok(())
            } else {
                Err(RuntimeError("not an object".to_string()))
            }
        }
    }
}

fn execute_function(
    context: &mut Context,
    args: Vec<String>,
    body: Expression,
    values: Vec<VariableValue>,
    layer: usize,
) -> Result<VariableValue, RuntimeError> {
    let mut inner_context = context.create_subcontext(layer)?;
    for i in 0..values.len() {
        inner_context.define_var(&args[i], values[i].clone())?;
    }
    let result = evaluate_expr(&mut inner_context, body)?;
    context.apply_subcontext(layer, inner_context)?;
    Ok(result)
}
