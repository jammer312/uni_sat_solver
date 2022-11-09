use core::fmt::Formatter;
use core::ops;
use std::{fmt::Display, marker::PhantomData, vec};

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

impl ops::Not for Value {
    type Output = Value;

    fn not(self) -> Value {
        match self {
            Value::Some(x) => Value::Some(!x),
            _ => Value::Any,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Some(value) => write!(f, "{value}"),
            Value::Any => write!(f, "any"),
        }
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Self {
        Value::Some(x)
    }
}

pub trait Computable<State, Result> {
    fn compute(&self, state: &State) -> Result;
}

#[derive(Clone)]
pub struct Conjunct<Atom, State> {
    pub atoms: vec::Vec<Atom>,
    state_type: PhantomData<*const State>,
}

impl<Atom: Computable<State, Value>, State> Conjunct<Atom, State> {
    pub fn new(atoms: vec::Vec<Atom>) -> Conjunct<Atom, State> {
        Conjunct::<Atom, State> {
            atoms,
            state_type: PhantomData,
        }
    }
}

impl<Atom: Display, State> Display for Conjunct<Atom, State> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut first = true;
        self.atoms.iter().try_for_each(|disjunct| {
            if !first {
                write!(f, " && ")?
            };
            first = false;
            write!(f, "({disjunct})")
        })
    }
}

impl<State, Atom: Computable<State, Value>> Computable<State, Value> for Conjunct<Atom, State> {
    fn compute(&self, state: &State) -> Value {
        self.atoms
            .iter()
            .fold(Value::Some(true), |acc, next| acc & next.compute(state))
    }
}

#[derive(Clone)]
pub struct Disjunct<Atom: Clone, State: Clone> {
    pub atoms: vec::Vec<Atom>,
    state_type: PhantomData<*const State>,
}

impl<Atom: Clone + Computable<State, Value>, State: Clone> Disjunct<Atom, State> {
    pub fn new(atoms: vec::Vec<Atom>) -> Disjunct<Atom, State> {
        Disjunct::<Atom, State> {
            atoms,
            state_type: PhantomData,
        }
    }
}

impl<Atom: Clone + Display, State: Clone> Display for Disjunct<Atom, State> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut first = true;
        self.atoms.iter().try_for_each(|atom| {
            if !first {
                write!(f, " || ")?
            };
            first = false;
            write!(f, "{atom}")
        })
    }
}

impl<State: Clone, Atom: Clone + Computable<State, Value>> Computable<State, Value>
    for Disjunct<Atom, State>
{
    fn compute(&self, state: &State) -> Value {
        self.atoms
            .iter()
            .fold(Value::Some(false), |acc, next| acc | next.compute(state))
    }
}
