#[derive(Debug, thiserror::Error)]
pub(crate) enum Error {
    #[error("failed to parse openapi spec")]
    FailedToParseOpenAPISpec,

    #[error("{0:?}")]
    FromUtf8Error(std::string::FromUtf8Error),

    #[error("input file invalid")]
    InputFileInvalid,

    #[error("input not specified")]
    InputNotSpecified,

    #[error("{0:?}")]
    IO(std::io::Error),
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::IO(value)
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(value: std::string::FromUtf8Error) -> Self {
        Error::FromUtf8Error(value)
    }
}

pub(crate) type Result<T> = std::result::Result<T, Error>;
