use log::debug;

use crate::*;


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
        if let Some(stmnt) = try_get_assignment(t.clone()) {
            return Ok(stmnt);
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

fn try_get_assignment(t: Vec<Token>) -> Option<Statement> {
    let mut indent_depth = 0;
    let mut assign_pos = None;
    let mut opassign_pos = None;

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
                    assign_pos = Some(i);
                    break;
                }
            }
            Token::OperatorAssign(op) => {
                if indent_depth == 0 {
                    opassign_pos = Some((i, op));
                    break;
                }
            }
            _ => (),
        }
    }

    if let Some(pos) = assign_pos {
        let maybe_ref_expr = try_get_reference_expr(t[..pos].to_vec());
        let maybe_val_expr = try_get_expr(t[pos + 1..].to_vec());
        if let (Some(ref_expr), Some(val_expr)) = (maybe_ref_expr, maybe_val_expr) {
            Some(Statement::VariableAssignment(ref_expr, val_expr))
        } else {
            None
        }
    } else if let Some((pos, op)) = opassign_pos {
        let maybe_ref_expr = try_get_expr(t[..pos].to_vec());
        let maybe_val_expr = try_get_expr(t[pos + 1..].to_vec());
        if let (Some(Expression::Reference(ref_expr)), Some(val_expr)) =
            (maybe_ref_expr, maybe_val_expr)
        {
            Some(Statement::OperatorAssignment(ref_expr, val_expr, *op))
        } else {
            None
        }
    } else {
        None
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
