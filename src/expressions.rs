use crate::{variables::{VariableValue, Operator}, structs::Statement};

#[derive(Debug, Clone)]
pub enum Expression {
    Value(VariableValue),
    List(Vec<Expression>),
    Reference(ReferenceExpr),
    BinaryOperator(Box<Expression>, Box<Expression>, Operator),
    UnaryOperator(Box<Expression>, Operator),
    Block(Vec<Statement>),
    FunctionCall(ReferenceExpr, Vec<Expression>),
    IfElse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
}

#[derive(Debug, Clone)]
pub enum ReferenceExpr {
    Variable(String),
    Index(Box<ReferenceExpr>, Box<Expression>),
    Object(Box<ReferenceExpr>, String),
}
