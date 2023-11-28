use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::variables::*;

#[derive(Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Value(VariableValue),
    OpeningBrace,
    ClosingBrace,
    OpeningParethesis,
    ClosingParethesis,
    ClosingBracket,
    OpeningBracket,
    Semicolon,
    Assign,
    OperatorAssign(Operator),
    Comma,
    Quote,
    Dot,
    Operator(Operator),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stri = match self {
            Token::Assign => "=".to_string(),
            Token::OperatorAssign(op) => format!("={}", Token::Operator(*op)),
            Token::Comma => ",".to_string(),
            Token::Quote => "\"".to_string(),
            Token::OpeningBrace => "{".to_string(),
            Token::ClosingBrace => "}".to_string(),
            Token::OpeningParethesis => "(".to_string(),
            Token::ClosingParethesis => ")".to_string(),
            Token::OpeningBracket => "[".to_string(),
            Token::ClosingBracket => "]".to_string(),
            Token::Identifier(s) => s.to_string(),
            Token::Keyword(Keyword::Fn) => "fn".to_string(),
            Token::Keyword(Keyword::Let) => "let".to_string(),
            Token::Keyword(Keyword::Return) => "return".to_string(),
            Token::Keyword(Keyword::While) => "while".to_string(),
            Token::Keyword(Keyword::For) => "for".to_string(),
            Token::Keyword(Keyword::If) => "if".to_string(),
            Token::Keyword(Keyword::Else) => "else".to_string(),
            Token::Value(v) => v.to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Dot => ".".to_string(),
            Token::Operator(Operator::Add) => "+".to_string(),
            Token::Operator(Operator::Subtract) => "-".to_string(),
            Token::Operator(Operator::Multiply) => "*".to_string(),
            Token::Operator(Operator::Equal) => "==".to_string(),
            Token::Operator(Operator::LessThan) => "<".to_string(),
            Token::Operator(Operator::LessThanOrEqual) => "<=".to_string(),
            Token::Operator(Operator::GreaterThan) => ">".to_string(),
            Token::Operator(Operator::GreaterThanOrEqual) => ">=".to_string(),
            Token::Operator(Operator::NotEqual) => "!=".to_string(),
            Token::Operator(Operator::Not) => "!".to_string(),
            Token::Operator(Operator::Negate) => "-".to_string(),
            Token::Operator(Operator::UnaryPlus) => "+".to_string(),
        };
        f.write_str(&stri)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Let,
    While,
    For,
    Return,
    Fn,
    If,
    Else,
}

#[derive(Debug, Clone)]
pub enum CharToken {
    Char(char),
    Identifier(String),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Value(VariableValue),
    List(Vec<Expression>),
    Reference(String),
    BinaryOperator(Box<Expression>, Box<Expression>, Operator),
    UnaryOperator(Box<Expression>, Operator),
    Block(Vec<Statement>),
    FunctionCall(String, Vec<Expression>),
    IfElse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDefinition(String, Expression),
    VariableAssignment(String, Expression),
    OperatorAssignment(String, Expression, Operator),
    FunctionDefinition(String, Vec<String>, Expression),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    WhileLoop(Expression, Expression),
    ForLoop(Box<(Statement, Statement)>, Expression, Expression),
    Empty,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, VariableValue>,
}

#[derive(Debug)]
pub struct Context {
    pub layers: Vec<Scope>,
    pub cur_layer: usize,
}

impl Context {
    pub fn current_scope(&mut self) -> &mut Scope {
        &mut self.layers[self.cur_layer]
    }
    pub fn define_var(&mut self, var: &String, val: VariableValue) -> Result<(), RuntimeError> {
        self.current_scope().define_var(var, val)
    }

    pub fn try_get_var(&self, var: &String) -> Option<(usize, VariableValue)> {
        self.layers
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, layer)| layer.try_get_var(var).map(|v| (i, v)))
    }

    pub fn try_set_var(&mut self, var: &String, val: VariableValue) -> Option<()> {
        self.layers
            .iter_mut()
            .rev()
            .find_map(|layer| layer.try_set_var(var, val.clone()))
    }

    pub fn get_var(&self, var: &String) -> Result<VariableValue, RuntimeError> {
        self.try_get_var(var)
            .map(|(_, v)| v)
            .ok_or(RuntimeError(format!("Variable '{}' does not exist", var)))
    }

    pub fn get_var_layer(&self, var: &String) -> Result<usize, RuntimeError> {
        self.try_get_var(var)
            .map(|(i, _)| i)
            .ok_or(RuntimeError(format!("Variable '{}' does not exist", var)))
    }

    pub fn set_var(&mut self, var: &String, val: VariableValue) -> Result<(), RuntimeError> {
        self.try_set_var(var, val)
            .ok_or(RuntimeError(format!("Variable '{}' does not exist", var)))
    }

    pub fn create_fn_context(&self, var: &String) -> Result<Context, RuntimeError> {
        let i = self.get_var_layer(var)?;
        let mut new_layers = self.layers[0..i + 1].to_vec();
        new_layers.push(Scope::new());
        Ok(Context {
            layers: new_layers,
            cur_layer: i + 1,
        })
    }

    pub fn create_block_context(&self) -> Result<Context, RuntimeError> {
        let i = self.cur_layer;
        let mut new_layers = self.layers[0..i + 1].to_vec();
        new_layers.push(Scope::new());
        Ok(Context {
            layers: new_layers,
            cur_layer: i + 1,
        })
    }

    pub fn apply_fn_context(&mut self, var: &String, context: Context) -> Result<(), RuntimeError> {
        let i = self.get_var_layer(var)?;
        for scope_i in 0..i + 1 {
            let s = &mut self.layers[scope_i];
            let s2 = &context.layers[scope_i];
            for (key, val) in &s2.variables {
                s.try_set_var(key, val.clone())
                    .ok_or(RuntimeError(format!("Error applying context")))?;
            }
        }
        Ok(())
    }

    pub fn apply_block_context(&mut self, context: Context) -> Result<(), RuntimeError> {
        let i = self.cur_layer;
        for scope_i in 0..i + 1 {
            let s = &mut self.layers[scope_i];
            let s2 = &context.layers[scope_i];
            for (key, val) in &s2.variables {
                s.try_set_var(key, val.clone())
                    .ok_or(RuntimeError(format!("Error applying context")))?;
            }
        }
        Ok(())
    }
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

    pub fn try_set_var(&mut self, var: &String, val: VariableValue) -> Option<()> {
        if let Some(v) = self.variables.get_mut(var) {
            *v = val;
            Some(())
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError(pub String);

#[derive(Debug)]
pub struct SyntaxError(pub String);

#[derive(Debug)]
pub struct ClientError(pub String);
