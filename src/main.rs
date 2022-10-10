#![allow(dead_code)]

use nom::Finish;
use std::error;
use std::fmt;
use std::io;
use std::io::Read;
use std::str;

mod language;
mod parser;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    UTFConversion(str::Utf8Error),
    Parse(nom::error::Error<String>),
    Validation(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IO(..) => write!(f, "io error"),
            Error::UTFConversion(..) => write!(f, "urf8 conversion error"),
            Error::Parse(..) => write!(f, "parse error"),
            Error::Validation(reason) => write!(f, "validation error: {reason}"),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            Error::IO(ref err) => Some(err),
            Error::UTFConversion(ref err) => Some(err),
            Error::Parse(ref err) => Some(err),
            Error::Validation(ref _err) => None,
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

fn main() -> Result<(), Error> {
    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer)?;
    let data = str::from_utf8(&buffer)?;
    let cnf = parser::cnf(data).finish();
    match cnf {
        Ok((_, cnf)) => println!("{:?}", cnf?),
        Err(err) => println!("{:?}", err),
    }
    // let cnf = parser::cnf(str::from_utf8(&buffer)?)
    //     .map_err(|e| e.to_owned())
    //     .finish()
    //     .1;
    Ok(())
}
