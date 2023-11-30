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
            VariableValue::Object(m) => format!("Object({:?})", m),
            VariableValue::Number(n) => n.to_string(),
            VariableValue::Boolean(b) => b.to_string(),
            VariableValue::String(s) => s.to_string(),
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
    pub fn get_type(&self) -> String {
        match self {
            VariableValue::Boolean(_) => "Boolean",
            VariableValue::Number(_) => "Number",
            VariableValue::List(_) => "List",
            VariableValue::Function(_, _) => "Function",
            VariableValue::Unit => "Unit",
            VariableValue::String(_) => "String",
            VariableValue::Object(_) => "Object",
        }
        .to_string()
    }
    pub fn add(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Number(na + nb)),
            (Self::String(na), Self::String(nb)) => Ok(VariableValue::String(na + &nb)),
            (Self::List(mut na), Self::List(mut nb)) => {
                na.append(&mut nb);
                Ok(VariableValue::List(na))
            }
            (Self::List(mut na), other) => {
                na.push(other);
                Ok(VariableValue::List(na))
            }
            (x, y) => Err(RuntimeError(format!(
                "Addition between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn subtract(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Number(na - nb)),
            (x, y) => Err(RuntimeError(format!(
                "Subtraction between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn multiply(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Number(na * nb)),
            (x, y) => Err(RuntimeError(format!(
                "Multiplication between {} and {} is not implemented!",
                x, y
            ))),
        }
    }
    pub fn divide(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Number(na / nb)),
            (x, y) => Err(RuntimeError(format!(
                "Division between {} and {} is not implemented!",
                x, y
            ))),
        }
    }
    pub fn modulo(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Number(na % nb)),
            (x, y) => Err(RuntimeError(format!(
                "Division between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn equals(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Boolean(na == nb)),
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na == nb)),
            (Self::String(na), Self::String(nb)) => Ok(VariableValue::Boolean(na == nb)),
            (x, y) => Err(RuntimeError(format!(
                "Equal between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn not_equals(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Boolean(na != nb)),
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na != nb)),
            (Self::String(na), Self::String(nb)) => Ok(VariableValue::Boolean(na != nb)),
            (x, y) => Err(RuntimeError(format!(
                "Not Equal between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn less_than(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Boolean(na < nb)),
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na < nb)),
            (Self::String(na), Self::String(nb)) => Ok(VariableValue::Boolean(na < nb)),
            (x, y) => Err(RuntimeError(format!(
                "Less Than between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn greater_than(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Boolean(na > nb)),
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na > nb)),
            (Self::String(na), Self::String(nb)) => Ok(VariableValue::Boolean(na > nb)),
            (x, y) => Err(RuntimeError(format!(
                "Greater Than between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn less_than_or_equal(
        a: VariableValue,
        b: VariableValue,
    ) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Boolean(na <= nb)),
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na <= nb)),
            (Self::String(na), Self::String(nb)) => Ok(VariableValue::Boolean(na <= nb)),
            (x, y) => Err(RuntimeError(format!(
                "Less Than Or Equal between {} and {} is not implemented!",
                x, y
            ))),
        }
    }

    pub fn greater_than_or_equal(
        a: VariableValue,
        b: VariableValue,
    ) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Boolean(na >= nb)),
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na >= nb)),
            (Self::String(na), Self::String(nb)) => Ok(VariableValue::Boolean(na >= nb)),
            (x, y) => Err(RuntimeError(format!(
                "Greater Than Or Equal between {} and {} is not implemented!",
                x, y
            ))),
        }
    }
    pub fn and(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na && nb)),
            (x, y) => Err(RuntimeError(format!(
                "AND between {} and {} is not implemented!",
                x, y
            ))),
        }
    }
    pub fn or(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Boolean(na), Self::Boolean(nb)) => Ok(VariableValue::Boolean(na || nb)),
            (x, y) => Err(RuntimeError(format!(
                "OR between {} and {} is not implemented!",
                x, y
            ))),
        }
    }
    pub fn not(a: VariableValue) -> Result<VariableValue, RuntimeError> {
        match a {
            Self::Boolean(na) => Ok(VariableValue::Boolean(!na)),
            x => Err(RuntimeError(format!("Not for {} is not implemented!", x))),
        }
    }
    pub fn negate(a: VariableValue) -> Result<VariableValue, RuntimeError> {
        match a {
            Self::Number(na) => Ok(VariableValue::Number(-na)),
            x => Err(RuntimeError(format!("Not for {} is not implemented!", x))),
        }
    }
    pub fn unary_plus(a: VariableValue) -> Result<VariableValue, RuntimeError> {
        match a {
            Self::Number(na) => Ok(VariableValue::Number(0 + na)),
            x => Err(RuntimeError(format!(
                "Unary Plus for {} is not implemented!",
                x
            ))),
        }
    }
}
