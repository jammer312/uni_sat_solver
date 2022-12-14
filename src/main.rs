#![allow(dead_code)]
mod cdcl;
mod language;
mod smt_euf;

use nom::error::VerboseError;
use nom::Finish;
use std::error;
use std::fmt;
use std::io;
use std::io::Read;
use std::str;

#[derive(Debug)]
pub enum Error {
    CDCL(cdcl::Error),
    IO(io::Error),
    UTFConversion(str::Utf8Error),
    Parse(nom::error::Error<String>),
    DPLLT(String),
    ParseVerbose(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IO(..) => write!(f, "io error"),
            Error::UTFConversion(..) => write!(f, "urf8 conversion error"),
            Error::Parse(..) => write!(f, "parse error"),
            Error::ParseVerbose(reason) => write!(f, "parse error: {reason}"),
            Error::CDCL(..) => write!(f, "CDCL solver error"),
            Error::DPLLT(reason) => write!(f, "DPLLT solver error: {reason}"),
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
            Error::CDCL(ref err) => Some(err),
            Error::DPLLT(..) => None,
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

impl From<cdcl::Error> for Error {
    fn from(error: cdcl::Error) -> Error {
        Error::CDCL(error)
    }
}

pub fn main() -> Result<(), Error> {
    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer)?;
    let data = str::from_utf8(&buffer)?;
    let parsed = smt_euf::parser::formula::<VerboseError<&str>>(data).finish();
    match parsed {
        Ok((_, formula)) => {
            let solution = formula.solve()?;
            println!("{solution}");
            Ok(())
        }
        Err(err) => Ok(println!(
            "`Parse error: {}",
            nom::error::convert_error(data, err)
        )),
    }
    // .finish()?
    // .1;
    // Ok(println!("{}", formula.solve()?))
}
