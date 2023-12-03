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

impl<T> From<T> for SyntaxError
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        SyntaxError(value.into())
    }
}

impl<T> From<T> for ClientError
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        ClientError(value.into())
    }
}

impl<T> From<T> for RuntimeError
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        RuntimeError(value.into())
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

impl From<ClientError> for Error {
    fn from(value: ClientError) -> Self {
        Error::C(value)
    }
}
