use nom::Finish;
use solver::Solvable;
use std::error;
use std::fmt;
use std::io;
use std::io::Read;
use std::str;

mod language;
mod parser;
mod solver;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    UTFConversion(str::Utf8Error),
    Parse(nom::error::Error<String>),
    ParseVerbose(String),
    Validation(String),
    IncorrectInput(String),
    ConflictingImplication,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IO(..) => write!(f, "io error"),
            Error::UTFConversion(..) => write!(f, "urf8 conversion error"),
            Error::Parse(..) => write!(f, "parse error"),
            Error::ParseVerbose(reason) => write!(f, "parse error: {reason}"),
            Error::Validation(reason) => write!(f, "validation error: {reason}"),
            Error::IncorrectInput(reason) => write!(f, "incorrect input: {reason}"),
            Error::ConflictingImplication => write!(f, "tried to add conflicting implication"),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            Error::IO(ref err) => Some(err),
            Error::UTFConversion(ref err) => Some(err),
            Error::Parse(ref err) => Some(err),
            Error::ParseVerbose(..) => None,
            Error::Validation(..) => None,
            Error::IncorrectInput(..) => None,
            Error::ConflictingImplication => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error)
    }
}

impl From<str::Utf8Error> for Error {
    fn from(error: str::Utf8Error) -> Error {
        Error::UTFConversion(error)
    }
}

impl From<nom::error::Error<String>> for Error {
    fn from(error: nom::error::Error<String>) -> Error {
        Error::Parse(error)
    }
}

pub fn main() -> Result<(), Error> {
    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer)?;
    let data = str::from_utf8(&buffer)?;
    let cnf = parser::cnf(data)
        .map_err(|e| e.map(|e| Error::ParseVerbose(nom::error::convert_error(data, e.into()))))
        .finish()?
        .1?;
    // println!("{cnf}");
    Ok(println!("{}", cnf.solve()?))
}
