use std::fmt::Display;

use log::info;

use crate::*;
use unescaper::unescape;

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
    Ampersand,
    VerticalBar,
    OpeningBracket,
    Semicolon,
    Assign,
    OperatorAssign(Operator),
    Comma,
    Quote,
    Dot,
    Colon,
    Operator(Operator),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stri = match self {
            Token::Assign => "=".to_string(),
            Token::OperatorAssign(op) => format!("={}", Token::Operator(*op)),
            Token::Comma => ",".to_string(),
            Token::Quote => "\"".to_string(),
            Token::Colon => ":".to_string(),
            Token::OpeningBrace => "{".to_string(),
            Token::ClosingBrace => "}".to_string(),
            Token::OpeningParethesis => "(".to_string(),
            Token::ClosingParethesis => ")".to_string(),
            Token::OpeningBracket => "[".to_string(),
            Token::ClosingBracket => "]".to_string(),
            Token::Ampersand => "&".to_string(),
            Token::VerticalBar => "|".to_string(),
            Token::Identifier(s) => s.to_string(),
            Token::Keyword(Keyword::Let) => "let".to_string(),
            Token::Keyword(Keyword::Return) => "return".to_string(),
            Token::Keyword(Keyword::Break) => "break".to_string(),
            Token::Keyword(Keyword::Continue) => "continue".to_string(),
            Token::Keyword(Keyword::While) => "while".to_string(),
            Token::Keyword(Keyword::For) => "for".to_string(),
            Token::Keyword(Keyword::If) => "if".to_string(),
            Token::Keyword(Keyword::In) => "in".to_string(),
            Token::Keyword(Keyword::Else) => "else".to_string(),
            Token::Value(v) => v.to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Dot => ".".to_string(),
            Token::Operator(Operator::Add) => "+".to_string(),
            Token::Operator(Operator::Subtract) => "-".to_string(),
            Token::Operator(Operator::Multiply) => "*".to_string(),
            Token::Operator(Operator::Divide) => "/".to_string(),
            Token::Operator(Operator::Equal) => "==".to_string(),
            Token::Operator(Operator::LessThan) => "<".to_string(),
            Token::Operator(Operator::LessThanOrEqual) => "<=".to_string(),
            Token::Operator(Operator::GreaterThan) => ">".to_string(),
            Token::Operator(Operator::GreaterThanOrEqual) => ">=".to_string(),
            Token::Operator(Operator::NotEqual) => "!=".to_string(),
            Token::Operator(Operator::Not) => "!".to_string(),
            Token::Operator(Operator::Negate) => "-".to_string(),
            Token::Operator(Operator::UnaryPlus) => "+".to_string(),
            Token::Operator(Operator::And) => "&&".to_string(),
            Token::Operator(Operator::Or) => "||".to_string(),
            Token::Operator(Operator::Modulo) => "%".to_string(),
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
    If,
    Else,
    In,
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub enum CharToken {
    Char(char),
    Identifier(String),
    String(String),
}

pub fn tokenize(program: &str) -> Result<Vec<Token>, SyntaxError> {
    let char_tokens = preprocess(program.chars().collect());
    info!("initial tokens: {:?}", char_tokens);

    let mapped_tokens = map_tokens(char_tokens)?;
    info!("mapped: {:?}", mapped_tokens);

    let merged_tokens = token_merger(mapped_tokens);
    info!("merged: {:?}", merged_tokens);

    Ok(merged_tokens)
}

fn preprocess(tokens: Vec<char>) -> Vec<CharToken> {
    let mut tokens_with_strings = Vec::new();
    let mut cur_string: Option<String> = None;
    for i in 0..tokens.len() {
        cur_string = match tokens[i] {
            '"' => match cur_string {
                Some(str) => {
                    tokens_with_strings.push(CharToken::String(str.clone()));
                    None
                }
                None => Some(String::new()),
            },
            c => match cur_string {
                Some(mut str) => {
                    str.push(c);
                    Some(str)
                }
                None => {
                    tokens_with_strings.push(CharToken::Char(c));
                    None
                }
            },
        }
    }

    let mut tokens_without_comments = Vec::new();
    let mut cur_comment = false;
    for i in 0..tokens_with_strings.len() {
        match &tokens_with_strings[i] {
            CharToken::Char('#') => cur_comment = !cur_comment,
            CharToken::Char('\n') => cur_comment = false,
            tkn => {
                if !cur_comment {
                    tokens_without_comments.push(tkn.clone())
                }
            }
        }
    }

    let mut tokens_with_identifiers = Vec::new();
    let mut cur_identifier = None;
    for i in 0..tokens_without_comments.len() {
        match &tokens_without_comments[i] {
            CharToken::Char(c) => {
                if c.is_alphanumeric() || *c == '_' {
                    cur_identifier.get_or_insert(String::new()).push(*c);
                } else if let Some(s) = cur_identifier {
                    tokens_with_identifiers.push(CharToken::Identifier(s));
                    cur_identifier = None;
                    tokens_with_identifiers.push(CharToken::Char(*c));
                } else {
                    tokens_with_identifiers.push(CharToken::Char(*c));
                }
            }
            tkn => {
                tokens_with_identifiers.push(tkn.clone());
            }
        }
    }

    let tokens_without_whitespace = tokens_with_identifiers
        .into_iter()
        .filter(|tkn| match tkn {
            CharToken::Char(c) => !c.is_whitespace(),
            _ => true,
        })
        .collect();

    tokens_without_whitespace
}

fn map_tokens(tokens: Vec<CharToken>) -> Result<Vec<Token>, SyntaxError> {
    tokens
        .into_iter()
        .filter_map(|x| match x {
            CharToken::Char(c) => Some(map_char_token(c, x)),
            CharToken::Identifier(s) => Some(map_string_token(s)),
            CharToken::String(s) => {
                let unescaped_s = unescape(&s)
                    .map_err(|e| SyntaxError(format!("couldn't unescape '{}': {}", s, e)));
                let return_val = unescaped_s.map(|rs| Token::Value(VariableValue::String(rs)));
                Some(return_val)
            }
        })
        .collect()
}

fn map_char_token(c: char, token: CharToken) -> Result<Token, SyntaxError> {
    match c {
        '=' => Ok(Token::Assign),
        '+' => Ok(Token::Operator(Operator::Add)),
        '-' => Ok(Token::Operator(Operator::Subtract)),
        '*' => Ok(Token::Operator(Operator::Multiply)),
        '/' => Ok(Token::Operator(Operator::Divide)),
        '%' => Ok(Token::Operator(Operator::Modulo)),
        '<' => Ok(Token::Operator(Operator::LessThan)),
        '>' => Ok(Token::Operator(Operator::GreaterThan)),
        '!' => Ok(Token::Operator(Operator::Not)),
        ';' => Ok(Token::Semicolon),
        ',' => Ok(Token::Comma),
        ':' => Ok(Token::Colon),
        '{' => Ok(Token::OpeningBrace),
        '}' => Ok(Token::ClosingBrace),
        '[' => Ok(Token::OpeningBracket),
        ']' => Ok(Token::ClosingBracket),
        '(' => Ok(Token::OpeningParethesis),
        ')' => Ok(Token::ClosingParethesis),
        '&' => Ok(Token::Ampersand),
        '|' => Ok(Token::VerticalBar),
        '.' => Ok(Token::Dot),
        '"' => Ok(Token::Quote),
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
        "for" => Ok(Token::Keyword(Keyword::For)),
        "return" => Ok(Token::Keyword(Keyword::Return)),
        "break" => Ok(Token::Keyword(Keyword::Break)),
        "continue" => Ok(Token::Keyword(Keyword::Continue)),
        "if" => Ok(Token::Keyword(Keyword::If)),
        "in" => Ok(Token::Keyword(Keyword::In)),
        "else" => Ok(Token::Keyword(Keyword::Else)),
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

fn token_merger(tokens: Vec<Token>) -> Vec<Token> {
    let mut new_tokens = Vec::new();
    for i in 0..tokens.len() {
        let (prev, cur) = (new_tokens.last(), tokens.get(i));
        if let Some(cur_tkn) = cur {
            match prev {
                Some(Token::Assign) => match cur_tkn {
                    Token::Assign => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::Equal));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::Operator(Operator::Not)) => match cur_tkn {
                    Token::Assign => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::NotEqual));
                    }
                    Token::Operator(Operator::Not) => {
                        new_tokens.pop();
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::Operator(Operator::LessThan)) => match cur_tkn {
                    Token::Assign => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::LessThanOrEqual));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::Operator(Operator::GreaterThan)) => match cur_tkn {
                    Token::Assign => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::GreaterThanOrEqual));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::Operator(Operator::Subtract)) => match cur_tkn {
                    Token::Operator(Operator::Subtract) => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::Add));
                    }
                    Token::Operator(Operator::Add) => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::Subtract));
                    }
                    Token::Assign => {
                        new_tokens.pop();
                        new_tokens.push(Token::OperatorAssign(Operator::Subtract));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::Operator(Operator::Add)) => match cur_tkn {
                    Token::Operator(Operator::Subtract) => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::Subtract));
                    }
                    Token::Operator(Operator::Add) => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::Add));
                    }
                    Token::Assign => {
                        new_tokens.pop();
                        new_tokens.push(Token::OperatorAssign(Operator::Add));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::Operator(Operator::Multiply)) => match cur_tkn {
                    Token::Assign => {
                        new_tokens.pop();
                        new_tokens.push(Token::OperatorAssign(Operator::Multiply));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::Ampersand) => match cur_tkn {
                    Token::Ampersand => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::And));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                Some(Token::VerticalBar) => match cur_tkn {
                    Token::VerticalBar => {
                        new_tokens.pop();
                        new_tokens.push(Token::Operator(Operator::Or));
                    }
                    _ => new_tokens.push(cur_tkn.clone()),
                },
                _ => new_tokens.push(cur_tkn.clone()),
            }
        }
    }
    new_tokens
}
