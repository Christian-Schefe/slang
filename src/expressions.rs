use std::collections::HashMap;

use log::{debug, info};

use crate::{
    context::Scope,
    statements::{get_statement, get_statements, Statement},
    tokenizer::{Keyword, Token},
    variables::{Operator, VariableValue},
};

#[derive(Debug, Clone)]
pub enum Expression {
    Value(VariableValue),
    List(Vec<Expression>),
    Object(HashMap<String, Expression>),
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

pub fn try_get_expr(t: Vec<Token>) -> Option<Expression> {
    let r = if t.len() < 1 {
        None
    } else if let Some(expr) = try_get_reference_expr(t.clone()) {
        Some(Expression::Reference(expr))
    } else if t.len() == 1 {
        if let Some(Token::Value(val)) = t.get(0) {
            Some(Expression::Value(val.clone()))
        } else {
            None
        }
    } else if let Some(expr) = try_get_closure_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_parenthesized_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_object_value(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_block_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_array_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_function_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_if_else_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_op_expr(t.clone()) {
        Some(expr)
    } else {
        None
    };
    info!("Get Expr: {:?} -> {:?}", t, r);
    r
}

pub fn try_get_closure_expr(t: Vec<Token>) -> Option<Expression> {
    if t.len() >= 4 && matches!(t[0], Token::VerticalBar) {
        let mut params = Vec::new();
        let mut closing_bar = None;
        for i in 1..t.len() {
            match &t[i] {
                Token::Identifier(s) => {
                    params.push(s.to_owned());
                }
                Token::Comma => (),
                Token::VerticalBar => {
                    closing_bar = Some(i);
                    break;
                }
                _ => {
                    return None;
                }
            }
        }
        if let Some(stop_i) = closing_bar {
            if let Some(func_expr) = try_get_expr(t[stop_i + 1..].to_vec()) {
                return Some(Expression::Value(VariableValue::Function(
                    params,
                    Box::new(func_expr),
                )));
            } else if let Ok(stmnt) = get_statement(t[stop_i + 1..].to_vec()) {
                return Some(Expression::Value(VariableValue::Function(
                    params,
                    Box::new(Expression::Block(vec![stmnt])),
                )));
            }
        }
        None
    } else {
        None
    }
}

pub fn try_get_object_value(t: Vec<Token>) -> Option<Expression> {
    if t.len() >= 2
        && matches!(t[0], Token::OpeningBrace)
        && matches!(t[t.len() - 1], Token::ClosingBrace)
    {
        if t.len() == 2 {
            return Some(Expression::Value(VariableValue::Object(Scope {
                variables: HashMap::new(),
            })));
        }
        let mut indent_depth = 0;
        let mut colons = Vec::new();
        for i in 1..t.len() - 1 {
            match &t[i] {
                Token::OpeningBrace => indent_depth += 1,
                Token::OpeningParethesis => indent_depth += 1,
                Token::OpeningBracket => indent_depth += 1,

                Token::ClosingBrace => indent_depth -= 1,
                Token::ClosingParethesis => indent_depth -= 1,
                Token::ClosingBracket => indent_depth -= 1,
                Token::Colon => {
                    if indent_depth == 0 {
                        colons.push(i);
                    }
                }
                _ => (),
            }
        }

        if colons.len() == 0 {
            return None;
        }

        let mut key_val_pairs = HashMap::new();

        for i in 0..colons.len() {
            let key_pos = colons[i] - 1;
            let val_start = colons[i] + 1;
            if i + 1 < colons.len() {
                let val_end = colons[i + 1] - 2;
                if let (Some(Token::Identifier(key)), Some(expr), Some(Token::Comma)) = (
                    t.get(key_pos),
                    try_get_expr(t[val_start..val_end].to_vec()),
                    t.get(val_end),
                ) {
                    key_val_pairs.insert(key.to_string(), expr);
                } else {
                    return None;
                }
            } else {
                if let (Some(Token::Identifier(key)), Some(expr)) = (
                    t.get(key_pos),
                    try_get_expr(t[val_start..t.len() - 1].to_vec()),
                ) {
                    key_val_pairs.insert(key.to_string(), expr);
                } else {
                    return None;
                }
            }
        }

        Some(Expression::Object(key_val_pairs))
    } else {
        None
    }
}

pub fn try_get_object_access(t: Vec<Token>) -> Option<ReferenceExpr> {
    if t.len() < 3 {
        return None;
    }
    if let Some(Token::Dot) = t.get(t.len() - 2) {
        let pre_ref_opt = try_get_reference_expr(t[..t.len() - 2].to_vec());
        let property_opt = t.last();
        if let (Some(pre_ref), Some(Token::Identifier(property))) = (pre_ref_opt, property_opt) {
            Some(ReferenceExpr::Object(
                Box::new(pre_ref),
                property.to_owned(),
            ))
        } else {
            None
        }
    } else {
        None
    }
}

pub fn try_get_reference_expr(t: Vec<Token>) -> Option<ReferenceExpr> {
    let r = if t.len() == 1 {
        if let Some(Token::Identifier(val)) = t.get(0) {
            Some(ReferenceExpr::Variable(val.clone()))
        } else {
            None
        }
    } else if let Some(expr) = try_get_array_access(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_object_access(t.clone()) {
        Some(expr)
    } else {
        None
    };

    debug!("Get Ref Expr: {:?} -> {:?}", t, r);
    r
}

pub fn try_get_array_access(t: Vec<Token>) -> Option<ReferenceExpr> {
    if !matches!(t.last(), Some(Token::ClosingBracket)) {
        return None;
    }

    let mut opening_bracket = None;
    let mut indent_level = 0;

    for i in (0..t.len() - 1).rev() {
        match t.get(i) {
            Some(Token::ClosingBracket) => {
                indent_level += 1;
            }
            Some(Token::OpeningBracket) => {
                if indent_level == 0 {
                    opening_bracket = Some(i);
                    break;
                }
                indent_level -= 1;
            }
            _ => (),
        }
    }

    if let Some(opening_i) = opening_bracket {
        if let (Some(Expression::Reference(list)), Some(index_expr)) = (
            try_get_expr(t[..opening_i].to_vec()),
            try_get_expr(t[opening_i + 1..t.len() - 1].to_vec()),
        ) {
            return Some(ReferenceExpr::Index(Box::new(list), Box::new(index_expr)));
        }
    }

    None
}

pub fn try_get_array_expr(t: Vec<Token>) -> Option<Expression> {
    if let (Some(Token::OpeningBracket), Some(Token::ClosingBracket)) =
        (t.get(0), t.get(t.len() - 1))
    {
        if t.len() == 2 {
            return Some(Expression::Value(VariableValue::List(Vec::new())));
        }
        let mut indent_depth = 0;
        let mut commas = Vec::new();
        for i in 1..t.len() - 1 {
            match &t[i] {
                Token::OpeningBrace => indent_depth += 1,
                Token::OpeningParethesis => indent_depth += 1,
                Token::OpeningBracket => indent_depth += 1,

                Token::ClosingBrace => indent_depth -= 1,
                Token::ClosingParethesis => indent_depth -= 1,
                Token::ClosingBracket => indent_depth -= 1,
                Token::Comma => {
                    if indent_depth == 0 {
                        commas.push(i);
                    }
                }
                _ => (),
            }
        }

        if commas.len() == 0 {
            return try_get_expr(t[1..t.len() - 1].to_vec())
                .and_then(|expr| Some(Expression::List(vec![expr])));
        }

        let mut exprs = Vec::with_capacity(commas.len() - 1);

        for i in 0..=commas.len() {
            let start = if i == 0 { 1 } else { commas[i - 1] + 1 };
            let end = if i == commas.len() {
                t.len() - 1
            } else {
                commas[i]
            };
            if let Some(expr) = try_get_expr(t[start..end].to_vec()) {
                exprs.push(expr);
            } else {
                return None;
            }
        }

        Some(Expression::List(exprs))
    } else {
        None
    }
}

pub fn try_get_parenthesized_expr(t: Vec<Token>) -> Option<Expression> {
    if let (Some(Token::OpeningParethesis), Some(Token::ClosingParethesis)) =
        (t.get(0), t.get(t.len() - 1))
    {
        if t.len() == 2 {
            Some(Expression::Value(VariableValue::Unit))
        } else {
            try_get_expr(t[1..t.len() - 1].to_vec())
        }
    } else {
        None
    }
}

pub fn try_get_if_else_expr(t: Vec<Token>) -> Option<Expression> {
    if let (Some(Token::Keyword(Keyword::If)), Some(Token::OpeningParethesis)) =
        (t.get(0), t.get(1))
    {
        let mut indent_depth = 0;
        let mut closing_parenthesis = None;

        for i in 2..t.len() {
            match &t[i] {
                Token::OpeningParethesis => {
                    indent_depth += 1;
                }
                Token::ClosingParethesis => {
                    if indent_depth == 0 {
                        closing_parenthesis = Some(i);
                        break;
                    } else {
                        indent_depth -= 1;
                    }
                }
                _ => (),
            }
        }

        if let Some(stop_i) = closing_parenthesis {
            let mut else_pos = None;
            let mut indent_level = 0;
            for i in stop_i + 1..t.len() {
                match &t[i] {
                    Token::OpeningBrace => indent_level += 1,
                    Token::OpeningParethesis => indent_level += 1,
                    Token::OpeningBracket => indent_level += 1,

                    Token::ClosingBrace => indent_level -= 1,
                    Token::ClosingParethesis => indent_level -= 1,
                    Token::ClosingBracket => indent_level -= 1,

                    Token::Keyword(Keyword::Else) => {
                        if indent_level == 0 {
                            else_pos = Some(i);
                            break;
                        }
                    }
                    _ => (),
                }
            }

            if let Some(else_i) = else_pos {
                if let (Some(condition), Some(if_body), Some(else_body)) = (
                    try_get_expr(t[2..stop_i].to_vec()),
                    try_get_expr(t[stop_i + 1..else_i].to_vec()),
                    try_get_expr(t[else_i + 1..].to_vec()),
                ) {
                    return Some(Expression::IfElse(
                        Box::new(condition),
                        Box::new(if_body),
                        Some(Box::new(else_body)),
                    ));
                }
            } else {
                if let (Some(condition), Some(if_body)) = (
                    try_get_expr(t[2..stop_i].to_vec()),
                    try_get_expr(t[stop_i + 1..].to_vec()),
                ) {
                    return Some(Expression::IfElse(
                        Box::new(condition),
                        Box::new(if_body),
                        None,
                    ));
                }
            }
        }
    }
    None
}

pub fn try_get_op_expr(t: Vec<Token>) -> Option<Expression> {
    // if matches!(t[0], Token::Operator(Operator::Not)) {
    //     return try_get_expr(t[1..t.len()].to_vec())
    //         .map(|expr| Expression::UnaryOperator(Box::new(expr), Operator::Not));
    // } else if matches!(t[0], Token::Operator(Operator::Negate)) {
    //     return try_get_expr(t[1..t.len()].to_vec())
    //         .map(|expr| Expression::UnaryOperator(Box::new(expr), Operator::Negate));
    // }
    let mut min_precedence = None;
    let mut op_pos = None;
    let mut indent_level = 0;

    for i in 0..t.len() {
        match t[i] {
            Token::OpeningBrace => indent_level += 1,
            Token::OpeningParethesis => indent_level += 1,
            Token::OpeningBracket => indent_level += 1,

            Token::ClosingBrace => indent_level -= 1,
            Token::ClosingParethesis => indent_level -= 1,
            Token::ClosingBracket => indent_level -= 1,

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
        if let Some(right_expr) = try_get_expr(t[(i + 1)..].to_vec()) {
            if let Some(left_expr) = try_get_expr(t[..i].to_vec()) {
                info!("Get ops: {:?} {:?} {:?}", left_expr, op, right_expr);
                return Some(Expression::BinaryOperator(
                    Box::new(left_expr),
                    Box::new(right_expr),
                    op,
                ));
            } else if i == 0 {
                info!("Get ops: {:?} {:?}", op, right_expr);
                match op {
                    Operator::Subtract => {
                        return Some(Expression::UnaryOperator(
                            Box::new(right_expr),
                            Operator::Negate,
                        ))
                    }
                    Operator::Add => {
                        return Some(Expression::UnaryOperator(
                            Box::new(right_expr),
                            Operator::UnaryPlus,
                        ))
                    }
                    _ => return Some(Expression::UnaryOperator(Box::new(right_expr), op)),
                }
            }
        }
    }

    None
}

pub fn try_get_block_expr(t: Vec<Token>) -> Option<Expression> {
    if t.len() >= 2
        && matches!(t[0], Token::OpeningBrace)
        && matches!(t[t.len() - 1], Token::ClosingBrace)
    {
        let statements = get_statements(t[1..t.len() - 1].to_vec()).ok();
        return statements.and_then(|s| Some(Expression::Block(s)));
    }

    None
}

pub fn try_get_function_expr(t: Vec<Token>) -> Option<Expression> {
    if t.len() >= 3 && matches!(t[t.len() - 1], Token::ClosingParethesis) {
        let mut indent_level = 0;
        let mut opening_parenthesis = None;

        for i in 0..t.len() {
            match &t[i] {
                Token::OpeningBrace => indent_level += 1,
                Token::OpeningParethesis => {
                    if indent_level == 0 {
                        opening_parenthesis = Some(i);
                        break;
                    }
                    indent_level += 1;
                }
                Token::OpeningBracket => indent_level += 1,

                Token::ClosingBrace => indent_level -= 1,
                Token::ClosingParethesis => indent_level -= 1,
                Token::ClosingBracket => indent_level -= 1,
                _ => (),
            }
        }
        if let Some(opening_i) = opening_parenthesis {
            if let Some(ref_expr) = try_get_reference_expr(t[0..opening_i].to_vec()) {
                let mut params = Vec::new();
                let mut last_comma = opening_i + 1;
                let mut indent_level = 0;
                for i in opening_i + 1..t.len() - 1 {
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
                let last_tokens = t[last_comma..t.len() - 1].to_vec();
                let last_tokens_count = last_tokens.len();
                let last_expr = try_get_expr(last_tokens);

                if last_expr.is_some() {
                    params.push(last_expr.unwrap());
                } else if last_tokens_count > 0 {
                    return None;
                }

                return Some(Expression::FunctionCall(ref_expr, params));
            }
        }
    }

    None
}
