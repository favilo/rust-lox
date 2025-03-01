#[derive(thiserror::Error, Debug)]
pub enum Error {
    // #[error("Failed to read file {0}")]
    // ReadFile(String),
    // #[error("Unknown command: {0}")]
    // UnknownCommand(String),
    #[error("Error: {0}")]
    TokenizeError(String),
}
