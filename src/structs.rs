use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

#[derive(Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Value(VariableValue),
    OpeningBrace,
    ClosingBrace,
    OpeningParethesis,
    ClosingParethesis,
    Semicolon,
    Assign,
    Comma,
    Quote,
    Dot,
    Operator(Operator),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stri = match self {
            Token::Assign => "=".to_string(),
            Token::Comma => ",".to_string(),
            Token::Quote => "\"".to_string(),
            Token::OpeningBrace => "{".to_string(),
            Token::ClosingBrace => "}".to_string(),
            Token::OpeningParethesis => "(".to_string(),
            Token::ClosingParethesis => ")".to_string(),
            Token::Identifier(s) => s.to_string(),
            Token::Keyword(Keyword::Fn) => "fn".to_string(),
            Token::Keyword(Keyword::Let) => "let".to_string(),
            Token::Keyword(Keyword::Return) => "return".to_string(),
            Token::Keyword(Keyword::While) => "while".to_string(),
            Token::Keyword(Keyword::If) => "if".to_string(),
            Token::Keyword(Keyword::Else) => "else".to_string(),
            Token::Value(v) => v.to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Dot => ".".to_string(),
            Token::Operator(Operator::Add) => "+".to_string(),
            Token::Operator(Operator::Subtract) => "-".to_string(),
            Token::Operator(Operator::Multiply) => "*".to_string(),
        };
        f.write_str(&stri)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Let,
    While,
    Return,
    Fn,
    If,
    Else,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
}

impl Operator {
    pub fn precedence(&self) -> u32 {
        match self {
            Operator::Multiply => 1,
            Operator::Add => 0,
            Operator::Subtract => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CharToken {
    Char(char),
    Identifier(String),
    String(String, bool),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Value(VariableValue),
    Reference(String),
    ComputedValue(Box<Expression>, Box<Expression>, Operator),
    Block(Vec<Statement>),
    FunctionCall(String, Vec<Expression>),
    IfElse(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDefinition(String, Expression),
    VariableAssignment(String, Expression),
    FunctionDefinition(String, Vec<String>, Expression),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    WhileLoop(Expression, Expression),
    Empty,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, VariableValue>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
        }
    }
    pub fn define_var(&mut self, var: &String, val: VariableValue) -> Result<(), RuntimeError> {
        if self.variables.contains_key(var) {
            return Err(RuntimeError(format!("Variable '{}' already exists", var)));
        }
        self.variables.insert(var.to_string(), val);
        Ok(())
    }

    pub fn try_get_var(&self, var: &String) -> Option<VariableValue> {
        self.variables.get(var).cloned()
    }

    pub fn get_var(&self, var: &String) -> Result<VariableValue, RuntimeError> {
        self.try_get_var(var)
            .ok_or(RuntimeError(format!("Variable '{}' does not exist", var)))
    }

    pub fn set_var(&mut self, var: &String, val: VariableValue) -> Result<(), RuntimeError> {
        *self
            .variables
            .get_mut(var)
            .ok_or(RuntimeError(format!("Variable '{}' does not exist", var)))? = val;
        Ok(())
    }
}

#[derive(Debug)]
pub struct RuntimeError(pub String);

#[derive(Debug)]
pub struct SyntaxError(pub String);

#[derive(Debug, Clone)]
pub enum VariableValue {
    Number(i32),
    Boolean(bool),
    String(String),
    Void,
    Function(Vec<String>, Box<Expression>),
}

impl Display for VariableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stri = match self {
            VariableValue::Void => "void".to_string(),
            VariableValue::Number(n) => n.to_string(),
            VariableValue::Boolean(b) => b.to_string(),
            VariableValue::String(s) => s.to_string(),
            VariableValue::Function(args, expr) => format!("{:?} -> {:?}", args, expr),
        };
        f.write_str(&stri)
    }
}
