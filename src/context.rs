use std::fmt::Debug;

#[derive(Debug)]
pub struct RuntimeError(pub String);

#[derive(Debug)]
pub struct SyntaxError(pub String);

#[derive(Debug)]
pub struct ClientError(pub String);

#[derive(Debug)]
pub enum Error {
    C(ClientError),
    S(SyntaxError),
    R(RuntimeError),
}

impl From<&str> for SyntaxError {
    fn from(value: &str) -> Self {
        SyntaxError(value.to_string())
    }
}
impl From<String> for SyntaxError {
    fn from(value: String) -> Self {
        SyntaxError(value)
    }
}
impl From<&str> for RuntimeError {
    fn from(value: &str) -> Self {
        RuntimeError(value.to_string())
    }
}
impl From<String> for RuntimeError {
    fn from(value: String) -> Self {
        RuntimeError(value)
    }
}
impl From<&str> for ClientError {
    fn from(value: &str) -> Self {
        ClientError(value.to_string())
    }
}
impl From<String> for ClientError {
    fn from(value: String) -> Self {
        ClientError(value)
    }
}
impl From<ClientError> for Error {
    fn from(value: ClientError) -> Self {
        Error::C(value)
    }
}
impl From<RuntimeError> for Error {
    fn from(value: RuntimeError) -> Self {
        Error::R(value)
    }
}
impl From<SyntaxError> for Error {
    fn from(value: SyntaxError) -> Self {
        Error::S(value)
    }
}
