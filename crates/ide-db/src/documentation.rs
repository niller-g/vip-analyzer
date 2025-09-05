//! Documentation attribute related utilities.

/// Holds documentation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Documentation(String);

impl Documentation {
    pub fn new(s: String) -> Self {
        Documentation(s)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<Documentation> for String {
    fn from(Documentation(string): Documentation) -> Self {
        string
    }
}
