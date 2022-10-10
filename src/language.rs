use crate::Error;
use core::ops;
use std::vec;
#[derive(Copy, Clone)]
enum Value {
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

struct State {
    variables: vec::Vec<Value>,
}

#[derive(Debug)]
struct Literal {
    index: usize,
    inverted: bool,
}

impl Literal {
    fn new(literal: &isize) -> Literal {
        Literal {
            index: literal.unsigned_abs(),
            inverted: literal.signum() < 0,
        }
    }

    fn compute(&self, state: &State) -> Value {
        match state.variables.get(self.index) {
            Some(&x) => x ^ self.inverted.into(),
            None => Value::Any,
        }
    }
}

#[derive(Debug)]
pub struct Disjunct {
    literals: vec::Vec<Literal>,
}

impl Disjunct {
    pub fn new(clause: vec::Vec<isize>) -> Disjunct {
        Disjunct {
            literals: clause.iter().map(|literal| Literal::new(literal)).collect(),
        }
    }

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
}

#[derive(Debug)]
pub struct CNF {
    disjuncts: vec::Vec<Disjunct>,
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
        Ok(CNF {
            disjuncts: disjuncts,
        })
    }
    fn compute(&self, state: &State) -> Value {
        self.disjuncts
            .iter()
            .fold(Value::Some(true), |acc, next| acc & next.compute(state))
    }
}
