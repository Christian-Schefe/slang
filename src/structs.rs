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
pub enum CharToken {
    Char(char),
    Identifier(String),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Value(VariableValue),
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

#[derive(Debug, Clone)]
pub enum VariableValue {
    Number(i32),
    Boolean(bool),
    String(String),
    Unit,
    Function(Vec<String>, Box<Expression>),
}

impl Display for VariableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stri = match self {
            VariableValue::Unit => "unit".to_string(),
            VariableValue::Number(n) => n.to_string(),
            VariableValue::Boolean(b) => b.to_string(),
            VariableValue::String(s) => s.to_string(),
            VariableValue::Function(args, expr) => format!("{:?} -> {:?}", args, expr),
        };
        f.write_str(&stri)
    }
}

impl VariableValue {
    pub fn add(a: VariableValue, b: VariableValue) -> Result<VariableValue, RuntimeError> {
        match (a, b) {
            (Self::Number(na), Self::Number(nb)) => Ok(VariableValue::Number(na + nb)),
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
