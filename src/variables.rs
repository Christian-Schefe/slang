use std::fmt::Display;

use crate::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    NotEqual,
    Equal,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Not,
    Negate,
    UnaryPlus,
}

impl Operator {
    pub fn precedence(&self) -> u32 {
        match self {
            Operator::Not => 4,
            Operator::Negate => 3,
            Operator::UnaryPlus => 3,
            Operator::Multiply => 2,
            Operator::Add => 1,
            Operator::Subtract => 1,
            Operator::LessThan => 0,
            Operator::GreaterThan => 0,
            Operator::Equal => 0,
            Operator::NotEqual => 0,
            Operator::LessThanOrEqual => 0,
            Operator::GreaterThanOrEqual => 0,
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
}

impl Display for VariableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stri = match self {
            VariableValue::Unit => "unit".to_string(),
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
        }
        .to_string()
    }
    pub fn add(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Number(na + nb)),
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
