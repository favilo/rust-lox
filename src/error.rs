#[derive(thiserror::Error, Debug)]
pub enum Error {
    // #[error("Failed to read file {0}")]
    // ReadFile(String),
    // #[error("Unknown command: {0}")]
    // UnknownCommand(String),
    #[error("Tokenize Error: {0}")]
    TokenizeError(String),

    #[error("Parse Error: {0}")]
    ParseError(String),
}
