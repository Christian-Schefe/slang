use log::{debug, info};

use crate::*;
use unescaper::unescape;

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
                let unescaped_s = unescape(&s).map_err(|_| SyntaxError("".to_owned()));
                let return_val =
                    unescaped_s.and_then(|rs| Ok(Token::Value(VariableValue::String(rs))));
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
        '<' => Ok(Token::Operator(Operator::LessThan)),
        '>' => Ok(Token::Operator(Operator::GreaterThan)),
        '!' => Ok(Token::Operator(Operator::Not)),
        ';' => Ok(Token::Semicolon),
        ',' => Ok(Token::Comma),
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
        "fn" => Ok(Token::Keyword(Keyword::Fn)),
        "if" => Ok(Token::Keyword(Keyword::If)),
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
            Token::OpeningParethesis => {
                block_indent += 1;
            }
            Token::ClosingParethesis => {
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
    if last_semicolon < tokens.len() {
        if let Some(last_expr) = try_get_expr(tokens[last_semicolon..].to_vec()) {
            statements.push(Statement::ReturnStatement(last_expr));
            Ok(statements)
        } else if let Ok(last_statement) = get_statement(tokens[last_semicolon..].to_vec()) {
            statements.push(last_statement);
            Ok(statements)
        } else {
            Err(SyntaxError(format!(
                "Invalid last expression of length {}",
                tokens.len() - last_semicolon
            )))
        }
    } else {
        Ok(statements)
    }
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
            } else if let Some(Token::OperatorAssign(op)) = t.get(1) {
                if let Some(expr) = try_get_expr(t[2..].to_vec()) {
                    return Ok(Statement::OperatorAssignment(s.to_string(), expr, *op));
                }
            }
        }
        if let Some(stmnt) = try_get_array_index_assignment(t.clone()) {
            return Ok(stmnt);
        }
        if let Some(Token::Keyword(Keyword::Return)) = t.get(0) {
            if t.len() == 1 {
                return Ok(Statement::ReturnStatement(Expression::Value(
                    VariableValue::Unit,
                )));
            }
            if let Some(expr) = try_get_expr(t[1..].to_vec()) {
                return Ok(Statement::ReturnStatement(expr));
            }
        }
        if let Some(stmnt) = try_get_while_loop(t.clone()) {
            return Ok(stmnt);
        }
        if let Some(stmnt) = try_get_for_loop(t.clone()) {
            return Ok(stmnt);
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

fn try_get_array_index_assignment(t: Vec<Token>) -> Option<Statement> {
    let mut indent_depth = 0;
    let mut assign_op = None;

    for i in 0..t.len() {
        match &t[i] {
            Token::OpeningParethesis => {
                indent_depth += 1;
            }
            Token::ClosingParethesis => {
                indent_depth -= 1;
            }
            Token::OpeningBrace => {
                indent_depth += 1;
            }
            Token::ClosingBrace => {
                indent_depth -= 1;
            }
            Token::OpeningBracket => {
                indent_depth += 1;
            }
            Token::ClosingBracket => {
                indent_depth -= 1;
            }
            Token::Assign => {
                if indent_depth == 0 {
                    assign_op = Some(i);
                    break;
                }
            }
            _ => (),
        }
    }

    if let Some(assign_pos) = assign_op {
        if let (Some(Expression::Indexing(arr, index_expr)), Some(expr)) = (
            try_get_expr(t[..assign_pos].to_vec()),
            try_get_expr(t[assign_pos + 1..].to_vec()),
        ) {
            return Some(Statement::ArrayAssignment(arr, *index_expr, expr));
        }
    }

    None
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

fn try_get_while_loop(t: Vec<Token>) -> Option<Statement> {
    if let (Some(Token::Keyword(Keyword::While)), Some(Token::OpeningParethesis)) =
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
            if let (Some(condition), Some(body)) = (
                try_get_expr(t[2..stop_i].to_vec()),
                try_get_expr(t[stop_i + 1..].to_vec()),
            ) {
                return Some(Statement::WhileLoop(condition, body));
            }
        }
    }
    None
}

fn try_get_for_loop(t: Vec<Token>) -> Option<Statement> {
    if let (Some(Token::Keyword(Keyword::For)), Some(Token::OpeningParethesis)) =
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
            let statements = get_statements(t[2..stop_i].to_vec());
            if let (Some(s), Some(body)) = (statements.ok(), try_get_expr(t[stop_i + 1..].to_vec()))
            {
                if let (
                    Some(Statement::VariableDefinition(_, _)),
                    Some(Statement::ExpressionStatement(condition)),
                    Some(_),
                ) = (s.get(0), s.get(1), s.get(2))
                {
                    return Some(Statement::ForLoop(
                        Box::new((s[0].clone(), s[2].clone())),
                        condition.clone(),
                        body,
                    ));
                }
            }
        }
    }
    None
}

fn try_get_expr(t: Vec<Token>) -> Option<Expression> {
    let r = if t.len() < 1 {
        None
    } else if t.len() == 1 {
        if let Some(Token::Value(val)) = t.get(0) {
            Some(Expression::Value(val.clone()))
        } else if let Some(Token::Identifier(val)) = t.get(0) {
            Some(Expression::Reference(val.clone()))
        } else {
            None
        }
    } else if let Some(expr) = try_get_parenthesized_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_block_expr(t.clone()) {
        Some(expr)
    } else if let Some(expr) = try_get_array_access(t.clone()) {
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

fn try_get_array_access(t: Vec<Token>) -> Option<Expression> {
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
            return Some(Expression::Indexing(list, Box::new(index_expr)));
        }
    }

    None
}

fn try_get_array_expr(t: Vec<Token>) -> Option<Expression> {
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
                Token::OpeningBrace => {
                    indent_depth += 1;
                }
                Token::ClosingBrace => {
                    indent_depth -= 1;
                }
                Token::OpeningParethesis => {
                    indent_depth += 1;
                }
                Token::ClosingParethesis => {
                    indent_depth -= 1;
                }
                Token::OpeningBracket => {
                    indent_depth += 1;
                }
                Token::ClosingBracket => {
                    indent_depth -= 1;
                }
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

fn try_get_parenthesized_expr(t: Vec<Token>) -> Option<Expression> {
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

fn try_get_if_else_expr(t: Vec<Token>) -> Option<Expression> {
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
            for i in stop_i + 1..t.len() {
                match &t[i] {
                    Token::OpeningParethesis => {
                        indent_depth += 1;
                    }
                    Token::ClosingParethesis => {
                        indent_depth -= 1;
                    }
                    Token::OpeningBrace => {
                        indent_depth += 1;
                    }
                    Token::ClosingBrace => {
                        indent_depth -= 1;
                    }
                    Token::Keyword(Keyword::Else) => {
                        if indent_depth == 0 {
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

fn try_get_op_expr(t: Vec<Token>) -> Option<Expression> {
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
            if params.len() == 0 && t.len() > 3 {
                return None;
            } else {
                return Some(Expression::FunctionCall(s.clone(), params));
            }
        }
    }

    None
}
