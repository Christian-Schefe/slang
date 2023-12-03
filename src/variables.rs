use std::fmt::Display;

use crate::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    NotEqual,
    Equal,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    And,
    Or,
    Not,
    Negate,
    UnaryPlus,
    Modulo,
}

impl Operator {
    pub fn precedence(&self) -> u32 {
        match self {
            Operator::Not => 10,
            Operator::Negate => 8,
            Operator::UnaryPlus => 8,
            Operator::Multiply => 6,
            Operator::Divide => 6,
            Operator::Add => 4,
            Operator::Subtract => 4,
            Operator::LessThan => 2,
            Operator::GreaterThan => 2,
            Operator::Equal => 2,
            Operator::NotEqual => 2,
            Operator::LessThanOrEqual => 2,
            Operator::GreaterThanOrEqual => 2,
            Operator::And => 0,
            Operator::Or => 0,
            Operator::Modulo => 7,
        }
    }
}

#[derive(Debug, Clone)]
pub enum VariableValue {
    Number(i32),
    Boolean(bool),
    String(String),
    Unit,
    Function(Vec<String>, Box<Expression>),
    List(Vec<VariableValue>),
    Object(Scope),
}

impl Display for VariableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stri = match self {
            VariableValue::Unit => "()".to_string(),
            VariableValue::Object(m) => {
                let mut s = String::new();
                s.push('{');
                for (i, (key, val)) in m.variables.iter().enumerate() {
                    s.push_str(key);
                    s.push_str(": ");
                    s.push_str(&val.to_string());
                    if i + 1 < m.variables.len() {
                        s.push_str(", ");
                    }
                }
                s.push('}');
                s
            }
            VariableValue::Number(n) => n.to_string(),
            VariableValue::Boolean(b) => b.to_string(),
            VariableValue::String(s) => format!("\"{}\"", s),
            VariableValue::Function(args, expr) => format!("{:?} -> {:?}", args, expr),
            VariableValue::List(list) => {
                let mut s = String::new();
                s.push('[');
                for i in 0..list.len() {
                    if i > 0 {
                        s.push(',');
                        s.push(' ');
                    }
                    s.push_str(&list[i].to_string());
                }
                s.push(']');
                s
            }
        };
        f.write_str(&stri)
    }
}

impl VariableValue {
    pub fn call(&self, params: Vec<VariableValue>) -> Result<VariableValue, RuntimeError> {
        match self {
            VariableValue::Function(args, body) => {
                let mut variables = HashMap::new();
                for (var, val) in args.iter().zip(params) {
                    define_var_by_val(&mut variables, var, val)?;
                }
                eval_expr(&mut variables, body)
            }
            _ => Err("variable is not callable".into()),
        }
    }
}
