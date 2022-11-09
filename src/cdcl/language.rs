use super::Error;
use core::cmp::max;
use core::ops;
use std::fmt;
use std::fmt::Display;
use std::vec;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Value {
    Some(bool),
    Any,
}

impl ops::BitXor<Value> for Value {
    type Output = Value;

    fn bitxor(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Some(x), Value::Some(y)) => Value::Some(x ^ y),
            _ => Value::Any,
        }
    }
}

impl ops::BitOr<Value> for Value {
    type Output = Value;

    fn bitor(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Some(true), _) => Value::Some(true),
            (_, Value::Some(true)) => Value::Some(true),
            (Value::Some(false), Value::Some(false)) => Value::Some(false),
            _ => Value::Any,
        }
    }
}

impl ops::BitAnd<Value> for Value {
    type Output = Value;

    fn bitand(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Some(false), _) => Value::Some(false),
            (_, Value::Some(false)) => Value::Some(false),
            (Value::Some(true), Value::Some(true)) => Value::Some(true),
            _ => Value::Any,
        }
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Self {
        Value::Some(x)
    }
}

#[derive(Clone, Debug)]
pub struct State {
    pub variables: vec::Vec<Value>,
}

impl State {
    #[allow(dead_code)]
    pub fn new(mut representation: vec::Vec<isize>) -> Result<State, Error> {
        representation.sort();
        representation.dedup();
        let max_index = representation
            .iter()
            .fold(0, |acc, next| max(acc, next.unsigned_abs()));
        let mut variables = vec::Vec::<Value>::with_capacity(max_index);
        variables.resize(max_index, Value::Any);
        representation
            .iter()
            .try_for_each(|v| match (v, v.signum() > 0) {
                (0, ..) => Err(Error::IncorrectInput(
                    "state init vec cannot contain zeros".to_owned(),
                )),
                (index, value) => {
                    let index = index.unsigned_abs() - 1;
                    match variables[index] {
                        Value::Some(_) => Err(Error::IncorrectInput(
                            "state init vec cannot contain conflicting values".to_owned(),
                        )),
                        Value::Any => Ok(variables[index] = value.into()),
                    }
                }
            })?;
        Ok(State { variables })
    }

    pub fn new_empty(size: usize) -> State {
        State {
            variables: vec![Value::Any; size],
        }
    }

    pub fn add(&mut self, literal: &Literal) -> Result<(), Error> {
        if self.variables.len() < literal.index + 1 {
            self.variables.resize(literal.index + 1, Value::Any)
        };
        match self.variables[literal.index] {
            Value::Any => Ok(self.variables[literal.index] = Value::Some(!literal.inverted)),
            Value::Some(x) => {
                if x == literal.inverted {
                    Err(Error::ConflictingImplication)
                } else {
                    Ok(())
                }
            }
        }
    }

    pub fn remove(&mut self, literal: &Literal) {
        match self.variables.get(literal.index) {
            None => (),
            Some(_) => self.variables[literal.index] = Value::Any,
        }
    }

    pub fn is_filled(&self) -> bool {
        for variable in &self.variables {
            match variable {
                Value::Any => return false,
                _ => (),
            }
        }
        return true;
    }

    fn as_literals(&self) -> vec::Vec<Literal> {
        self.variables
            .iter()
            .enumerate()
            .filter_map(|pair| match pair {
                (i, Value::Some(v)) => Some(Literal {
                    index: i,
                    inverted: !v,
                }),
                _ => None,
            })
            .collect()
    }
}

impl Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.variables
            .iter()
            .enumerate()
            .try_for_each(|pair| match pair {
                (index, Value::Some(true)) => write!(f, "x{} ", index + 1),
                (index, Value::Some(false)) => write!(f, "^x{} ", index + 1),
                _ => Ok(()),
            })
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq, PartialOrd, Ord)]
pub struct Literal {
    pub index: usize,
    pub inverted: bool,
}

impl Literal {
    pub fn new(literal: &isize) -> Literal {
        Literal {
            index: literal.unsigned_abs() - 1,
            inverted: literal.signum() < 0,
        }
    }

    pub fn compute(&self, state: &State) -> Value {
        match state.variables.get(self.index) {
            Some(&x) => x ^ self.inverted.into(),
            None => Value::Any,
        }
    }

    pub fn inverted(&self) -> Self {
        return Self {
            inverted: !self.inverted,
            ..*self
        };
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let index = self.index + 1;
        if self.inverted {
            write!(f, "^x{index}")
        } else {
            write!(f, "x{index}")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Disjunct {
    pub literals: vec::Vec<Literal>,
}

impl Disjunct {
    pub fn new(mut representation: vec::Vec<isize>) -> Disjunct {
        representation.sort_by(|a, b| a.abs().cmp(&b.abs()));
        representation.dedup();
        Disjunct {
            literals: representation
                .iter()
                .map(|literal| Literal::new(literal))
                .collect(),
        }
    }

    #[allow(dead_code)]
    fn compute(&self, state: &State) -> Value {
        self.literals
            .iter()
            .fold(Value::Some(false), |acc, next| acc | next.compute(state))
    }

    fn validate_height(&self, max_height: &usize) -> Result<(), Error> {
        Ok(self.literals.iter().try_for_each(|l| {
            if l.index > *max_height {
                return Err(Error::Validation(format!(
                    "found incorrect index for literal (got {}, must not be higher than {})",
                    l.index, max_height
                )));
            };
            Ok(())
        })?)
    }

    fn width(&self) -> usize {
        self.literals
            .iter()
            .fold(0, |acc, next| max(acc, next.index + 1))
    }
}

impl Display for Disjunct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut first = true;
        self.literals.iter().try_for_each(|literal| {
            if !first {
                write!(f, "|")?
            };
            first = false;
            let index = literal.index + 1;
            if literal.inverted {
                write!(f, "^x{index}")
            } else {
                write!(f, "x{index}")
            }
        })
    }
}

impl From<&State> for Disjunct {
    fn from(x: &State) -> Self {
        Self {
            literals: x.as_literals(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CNF {
    pub disjuncts: vec::Vec<Disjunct>,
}

impl CNF {
    pub fn new(
        header: Option<(usize, usize)>,
        disjuncts: vec::Vec<Disjunct>,
    ) -> Result<CNF, Error> {
        if let Some((variables, clauses)) = header {
            if !(disjuncts.len() == clauses) {
                return Err(Error::Validation(format!(
                    "incorrect number of clauses (expected {}, got {}",
                    clauses,
                    disjuncts.len()
                )));
            };
            disjuncts
                .iter()
                .try_for_each(|d| d.validate_height(&variables))?
        };
        Ok(CNF { disjuncts })
    }

    #[allow(dead_code)]
    fn compute(&self, state: &State) -> Value {
        self.disjuncts
            .iter()
            .fold(Value::Some(true), |acc, next| acc & next.compute(state))
    }

    pub fn width(&self) -> usize {
        self.disjuncts
            .iter()
            .fold(0, |acc, next| max(acc, next.width()))
    }
}

impl Display for CNF {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut first = true;
        self.disjuncts.iter().try_for_each(|disjunct| {
            if !first {
                write!(f, "&")?
            }
            first = false;
            write!(f, "({disjunct})")
        })
    }
}
