use log::{debug, info};

use crate::{SyntaxError, VariableValue};

#[derive(Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Value(VariableValue),
    OpeningBrace,
    ClosingBrace,
    Semicolon,
    Dot,
    Operator(Operator),
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Let,
    While,
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Operator {
    Assign,
    Add,
    Subtract,
    Multiply,
}

impl Operator {
    fn precedence(&self) -> u32 {
        match self {
            Operator::Multiply => 1,
            Operator::Add => 0,
            Operator::Subtract => 0,
            Operator::Assign => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CharToken {
    Char(char),
    String(String),
}

#[derive(Debug)]
pub enum Expression {
    Value(VariableValue),
    Reference(String),
    ComputedValue(Box<Expression>, Box<Expression>, Operator),
    Block(Vec<Statement>),
}

#[derive(Debug)]
pub enum Statement {
    VariableAssignment(String, Expression),
    ReturnStatement(Expression),
    Empty,
}

pub fn tokenize(program: &str) -> Result<Vec<Token>, SyntaxError> {
    let chars = program.chars().map(|c| CharToken::Char(c));

    let reduced_tokens = chars.fold(Vec::new(), char_reducer);
    info!("reduced: {:?}", reduced_tokens);

    let filtered_tokens: Vec<CharToken> = reduced_tokens
        .into_iter()
        .filter(|x| {
            if let CharToken::Char(c) = x {
                !c.is_whitespace()
            } else {
                true
            }
        })
        .collect();
    info!("filtered: {:?}", filtered_tokens);

    let mapped_tokens = map_tokens(filtered_tokens)?;
    info!("mapped: {:?}", mapped_tokens);
    Ok(mapped_tokens)
}

fn char_reducer(mut acc: Vec<CharToken>, cur: CharToken) -> Vec<CharToken> {
    if let Some(last_token) = acc.pop() {
        if let CharToken::Char(c) = cur {
            if c.is_ascii_alphanumeric() {
                if let CharToken::Char(lc) = last_token {
                    if lc.is_ascii_alphanumeric() {
                        let mut str = String::new();
                        str.push(lc);
                        str.push(c);
                        acc.push(CharToken::String(str));
                    } else {
                        acc.push(CharToken::Char(lc));
                        acc.push(CharToken::Char(c));
                    }
                } else if let CharToken::String(mut lc) = last_token {
                    lc.push(c);
                    acc.push(CharToken::String(lc));
                }
            } else {
                acc.push(last_token);
                acc.push(CharToken::Char(c));
            }
        }
    } else {
        acc.push(cur);
    }
    acc
}

fn map_tokens(tokens: Vec<CharToken>) -> Result<Vec<Token>, SyntaxError> {
    tokens
        .into_iter()
        .map(|x| match x {
            CharToken::Char(c) => map_char_token(c, x),
            CharToken::String(s) => map_string_token(s),
        })
        .collect()
}

fn map_char_token(c: char, token: CharToken) -> Result<Token, SyntaxError> {
    match c {
        '=' => Ok(Token::Operator(Operator::Assign)),
        '+' => Ok(Token::Operator(Operator::Add)),
        '-' => Ok(Token::Operator(Operator::Subtract)),
        '*' => Ok(Token::Operator(Operator::Multiply)),
        ';' => Ok(Token::Semicolon),
        '{' => Ok(Token::OpeningBrace),
        '}' => Ok(Token::ClosingBrace),
        '.' => Ok(Token::Dot),
        chr => {
            if chr.is_ascii_alphabetic() {
                Ok(Token::Identifier(chr.to_string()))
            } else if let Some(d) = chr.to_digit(10) {
                Ok(Token::Value(VariableValue::Number(d as i32)))
            } else {
                Err(SyntaxError(format!("Invalid Token: {:?}", token)))
            }
        }
    }
}
fn map_string_token(s: String) -> Result<Token, SyntaxError> {
    match s.as_str() {
        "let" => Ok(Token::Keyword(Keyword::Let)),
        "while" => Ok(Token::Keyword(Keyword::While)),
        "return" => Ok(Token::Keyword(Keyword::Return)),
        "true" => Ok(Token::Value(VariableValue::Boolean(true))),
        "false" => Ok(Token::Value(VariableValue::Boolean(false))),
        str => {
            if let Ok(num) = str::parse(str) {
                Ok(Token::Value(VariableValue::Number(num)))
            } else {
                Ok(Token::Identifier(str.to_owned()))
            }
        }
    }
}

pub fn get_statements(tokens: Vec<Token>) -> Result<Vec<Statement>, SyntaxError> {
    let mut block_indent = 0;
    let mut last_semicolon = 0;
    let mut statements = Vec::new();

    for i in 0..tokens.len() {
        match tokens[i] {
            Token::OpeningBrace => {
                block_indent += 1;
            }
            Token::ClosingBrace => {
                block_indent -= 1;
            }
            Token::Semicolon => {
                if block_indent == 0 {
                    let s = get_statement(tokens[last_semicolon..i].to_vec())?;
                    statements.push(s);
                    last_semicolon = i + 1;
                }
            }
            _ => {}
        }
    }
    Ok(statements)
}

fn get_statement(t: Vec<Token>) -> Result<Statement, SyntaxError> {
    if t.len() == 0 {
        Ok(Statement::Empty)
    } else {
        debug!("Get Statement: {:?}", t);
        if let Some(Token::Keyword(Keyword::Let)) = t.get(0) {
            if let Some(Token::Identifier(s)) = t.get(1) {
                if let Some(Token::Operator(Operator::Assign)) = t.get(2) {
                    if let Some(expr) = try_get_expr(t[3..].to_vec()) {
                        return Ok(Statement::VariableAssignment(s.to_string(), expr));
                    }
                }
            }
        }
        if let Some(Token::Keyword(Keyword::Return)) = t.get(0) {
            if let Some(expr) = try_get_expr(t[1..].to_vec()) {
                return Ok(Statement::ReturnStatement(expr));
            }
        }
        Err(SyntaxError(format!("Invalid Statement: {:?}", t)))
    }
}

fn try_get_expr(t: Vec<Token>) -> Option<Expression> {
    debug!("Get Expr: {:?}", t);
    if t.len() == 1 {
        if let Some(Token::Value(val)) = t.get(0) {
            Some(Expression::Value(*val))
        } else if let Some(Token::Identifier(val)) = t.get(0) {
            Some(Expression::Reference(val.clone()))
        } else {
            None
        }
    } else if let Some(expr) = try_get_block_expr(t.clone()) {
        Some(expr)
    } else {
        let ops: Vec<(usize, Operator)> = t
            .iter()
            .cloned()
            .enumerate()
            .filter_map(|(i, x)| {
                if let Token::Operator(op) = x {
                    Some((i, op))
                } else {
                    None
                }
            })
            .collect();
        let min_prec = ops.iter().map(|(_, op)| op.precedence()).min();

        if min_prec.is_none() {
            return None;
        }

        let (i, op) = ops
            .iter()
            .rev()
            .find(|(_, op)| op.precedence() == min_prec.unwrap())
            .unwrap();

        if let Some(left_expr) = try_get_expr(t[..*i].to_vec()) {
            if let Some(right_expr) = try_get_expr(t[(*i + 1)..].to_vec()) {
                Some(Expression::ComputedValue(
                    Box::new(left_expr),
                    Box::new(right_expr),
                    *op,
                ))
            } else {
                None
            }
        } else {
            None
        }
    }
}

fn try_get_block_expr(t: Vec<Token>) -> Option<Expression> {
    if t.len() >= 2
        && matches!(t[0], Token::OpeningBrace)
        && matches!(t[t.len() - 1], Token::ClosingBrace)
    {
        let statements = get_statements(t[1..t.len() - 1].to_vec()).ok();
        return statements.and_then(|s| Some(Expression::Block(s)));
    }

    None
}
