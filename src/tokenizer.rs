use log::{debug, info};

use crate::*;

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
        '=' => Ok(Token::Assign),
        '+' => Ok(Token::Operator(Operator::Add)),
        '-' => Ok(Token::Operator(Operator::Subtract)),
        '*' => Ok(Token::Operator(Operator::Multiply)),
        ';' => Ok(Token::Semicolon),
        ',' => Ok(Token::Comma),
        '{' => Ok(Token::OpeningBrace),
        '}' => Ok(Token::ClosingBrace),
        '(' => Ok(Token::OpeningParethesis),
        ')' => Ok(Token::ClosingParethesis),
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
        "fn" => Ok(Token::Keyword(Keyword::Fn)),
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
                if let Some(Token::Assign) = t.get(2) {
                    if let Some(expr) = try_get_expr(t[3..].to_vec()) {
                        return Ok(Statement::VariableDefinition(s.to_string(), expr));
                    }
                }
            }
        }
        if let Some(Token::Identifier(s)) = t.get(0) {
            if let Some(Token::Assign) = t.get(1) {
                if let Some(expr) = try_get_expr(t[2..].to_vec()) {
                    return Ok(Statement::VariableAssignment(s.to_string(), expr));
                }
            }
        }
        if let Some(Token::Keyword(Keyword::Return)) = t.get(0) {
            if let Some(expr) = try_get_expr(t[1..].to_vec()) {
                return Ok(Statement::ReturnStatement(expr));
            }
        }
        if let Some(stmnt) = try_get_function_definition_statement(t.clone()) {
            return Ok(stmnt);
        }
        if let Some(expr) = try_get_expr(t.clone()) {
            return Ok(Statement::ExpressionStatement(expr));
        }
        Err(SyntaxError(format!("Invalid Statement: {:?}", t)))
    }
}

fn try_get_function_definition_statement(t: Vec<Token>) -> Option<Statement> {
    if let (
        Some(Token::Keyword(Keyword::Fn)),
        Some(Token::Identifier(s)),
        Some(Token::OpeningParethesis),
    ) = (t.get(0), t.get(1), t.get(2))
    {
        let mut params = Vec::new();
        let mut closing_parenthesis = None;
        for i in 3..t.len() {
            match &t[i] {
                Token::Identifier(s) => {
                    params.push(s.to_owned());
                }
                Token::Comma => (),
                Token::ClosingParethesis => {
                    closing_parenthesis = Some(i);
                    break;
                }
                _ => {
                    return None;
                }
            }
        }
        if let Some(stop_i) = closing_parenthesis {
            if let Some(func_expr) = try_get_expr(t[stop_i + 1..].to_vec()) {
                return Some(Statement::FunctionDefinition(
                    s.to_string(),
                    params,
                    func_expr,
                ));
            }
        }
    }
    None
}

fn try_get_expr(t: Vec<Token>) -> Option<Expression> {
    // debug!("Get Expr: {:?}", t);
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
    } else if let Some(expr) = try_get_function_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_op_expr(t.clone()) {
        Some(expr)
    } else {
        None
    }
}

fn try_get_op_expr(t: Vec<Token>) -> Option<Expression> {
    if matches!(t[0], Token::OpeningParethesis)
        && matches!(t[t.len() - 1], Token::ClosingParethesis)
    {
        return try_get_op_expr(t[1..t.len() - 1].to_vec());
    }
    let mut min_precedence = None;
    let mut op_pos = None;
    let mut indent_level = 0;

    for i in 0..t.len() {
        match t[i] {
            Token::OpeningBrace => indent_level += 1,
            Token::ClosingBrace => indent_level -= 1,
            Token::OpeningParethesis => indent_level += 1,
            Token::ClosingParethesis => indent_level -= 1,
            Token::Operator(op) => {
                let precedence = op.precedence();
                if indent_level == 0 && !min_precedence.is_some_and(|min_val| min_val < precedence)
                {
                    min_precedence = Some(precedence);
                    op_pos = Some((i, op));
                }
            }
            _ => (),
        }
    }

    if indent_level != 0 {
        return None;
    }
    if let Some((i, op)) = op_pos {
        if let Some(left_expr) = try_get_expr(t[..i].to_vec()) {
            if let Some(right_expr) = try_get_expr(t[(i + 1)..].to_vec()) {
                debug!("Get ops: {:?} {:?} {:?}", left_expr, op, right_expr);
                return Some(Expression::ComputedValue(
                    Box::new(left_expr),
                    Box::new(right_expr),
                    op,
                ));
            }
        }
    }

    None
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

fn try_get_function_expr(t: Vec<Token>) -> Option<Expression> {
    if t.len() >= 3
        && matches!(t[1], Token::OpeningParethesis)
        && matches!(t[t.len() - 1], Token::ClosingParethesis)
    {
        if let Token::Identifier(s) = &t[0] {
            debug!("func");
            let mut params = Vec::new();
            let mut last_comma = 2;
            let mut indent_level = 0;
            for i in 2..t.len() - 1 {
                match t[i] {
                    Token::Comma => {
                        if indent_level == 0 {
                            let expr = try_get_expr(t[last_comma..i].to_vec());
                            if expr.is_none() {
                                return None;
                            }
                            params.push(expr.unwrap());
                            last_comma = i + 1;
                        }
                    }
                    Token::OpeningParethesis => {
                        indent_level += 1;
                    }
                    Token::ClosingParethesis => {
                        indent_level -= 1;
                    }
                    _ => (),
                }
            }
            let last_expr = try_get_expr(t[last_comma..t.len() - 1].to_vec());
            if last_expr.is_some() {
                params.push(last_expr.unwrap());
            }
            return Some(Expression::FunctionCall(s.clone(), params));
        }
    }

    None
}
