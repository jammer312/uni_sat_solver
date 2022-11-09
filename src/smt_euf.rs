use bimap::BiMap;
use itertools::EitherOrBoth::{Both, Left, Right};
use itertools::Itertools;
use std::cmp::max;
use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;

use crate::language::Computable;
use crate::language::Value;

#[derive(Eq, Hash, PartialEq, Clone, Ord, PartialOrd, Debug)]
pub enum Atom {
    Eq(Term, Term),
    Neq(Term, Term),
}

impl Atom {
    fn order(&self) -> (usize, usize) {
        match self {
            Atom::Eq(a, b) => max(a.order(), b.order()),
            Atom::Neq(a, b) => max(a.order(), b.order()),
        }
    }
    fn inverted(&self) -> Atom {
        match self {
            Atom::Eq(a, b) => Atom::Neq(a.clone(), b.clone()),
            Atom::Neq(a, b) => Atom::Eq(a.clone(), b.clone()),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Eq(a, b) => write!(f, "({a} == {b})"),
            Atom::Neq(a, b) => write!(f, "({a} != {b})"),
        }
    }
}

impl<State: Computable<Atom, Value>> Computable<State, Value> for Atom {
    fn compute(&self, state: &State) -> Value {
        state.compute(self)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Ord, PartialOrd, Debug)]
pub enum Term {
    Identifier(String),
    Function(String, Vec<Term>),
}

impl Term {
    fn order(&self) -> (usize, usize) {
        match self {
            Term::Identifier(_) => (1, 0),
            Term::Function(_, vec) => vec
                .iter()
                .map(|t| t.order())
                .fold((0, 0), |acc, next| (acc.0 + next.0, max(acc.1, next.1))),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Identifier(name) => write!(f, "{name}"),
            Term::Function(name, terms) => {
                write!(f, "{name}(")?;
                let mut first = true;
                terms.iter().try_for_each(|term| {
                    if !first {
                        write!(f, ", ")?
                    }
                    first = false;
                    write!(f, "{term}")
                })?;
                write!(f, ")")
            }
        }
    }
}

#[derive(Clone)]
pub struct State {
    atoms: HashMap<Atom, Value>,
}

impl State {
    fn new() -> State {
        State {
            atoms: HashMap::new(),
        }
    }
    fn is_correct(&self) -> bool {
        let mut classes = EquivalenceClasses::new();
        let (mut eq, neq): (Vec<_>, Vec<_>) = self
            .atoms
            .iter()
            .filter_map(|(k, v)| {
                if let Value::Some(v) = v {
                    Some((k, *v))
                } else {
                    None
                }
            })
            .partition(|(k, v)| match (k, v) {
                (Atom::Eq(..), true) | (Atom::Neq(..), false) => true,
                _ => false,
            });
        eq.sort_by(|(a, _), (b, _)| a.cmp(&b));
        eq.sort_by(|(a, _), (b, _)| a.order().cmp(&b.order()));
        // println!("Atoms:");
        // eq.iter().for_each(|(rel, _)| match rel {
        //     Atom::Eq(..) => println!("{rel}"),
        //     _ => (),
        // });
        // neq.iter().for_each(|(rel, _)| match rel {
        //     Atom::Neq(..) => println!("{rel}"),
        //     _ => (),
        // });
        // println!();
        eq.iter().for_each(|(rel, _)| match rel {
            Atom::Eq(a, b) => classes.add_eq(a, b),
            _ => (),
        });
        // println!();
        // println!("{classes}");
        let conflict = neq.iter().find(|(rel, _)| match rel {
            Atom::Eq(a, b) => classes.has_eq(a, b),
            _ => false,
        });
        if let Some((Atom::Eq(..), _)) = conflict {
            // if let Some((Atom::Eq(l, r), _)) = conflict {
            // println!("Incorrect!\nResolvent:");
            // classes
            //     .build_resolvent(l, r)
            //     .iter()
            //     .for_each(|(l, r)| println!("{l} == {r}"));
            // println!("Raw resolvent:");
            // classes
            //     .build_resolvent_raw(l, r)
            //     .iter()
            //     .for_each(|(l, r)| println!("{l} == {r}"));
            return false;
        } else {
            return true;
        }
    }
    fn as_atoms(&self) -> Vec<Atom> {
        self.atoms
            .iter()
            .sorted_by_key(|x| x.0)
            .filter(|v| match v.0 {
                Atom::Eq(..) => true,
                _ => false,
            })
            .filter_map(|v| match v.1 {
                Value::Some(true) => Some(v.0.clone()),
                Value::Some(false) => Some(v.0.inverted()),
                _ => None,
            })
            .collect()
    }
}

impl Computable<Atom, Value> for State {
    fn compute(&self, atom: &Atom) -> Value {
        self.atoms.get(atom).map_or(Value::Any, |v| *v)
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "{{")?;
        self.as_atoms()
            .iter()
            .try_for_each(|atom| writeln!(f, "\t{atom}"))?;
        writeln!(f, "}}")
    }
}

#[derive(Debug)]
struct EquivalenceClasses {
    relations: Vec<usize>,
    // relations_raw: Vec<usize>,
    mapping: HashMap<TermClass, usize>,
    eqs: BiMap<(usize, usize), (Term, Term)>,
    // eqs_raw: BiMap<(usize, usize), (Term, Term)>,
}

impl EquivalenceClasses {
    fn new() -> EquivalenceClasses {
        EquivalenceClasses {
            relations: Vec::new(),
            // relations_raw: Vec::new(),
            mapping: HashMap::new(),
            eqs: BiMap::new(),
            // eqs_raw: BiMap::new(),
        }
    }

    fn add_eq(&mut self, a: &Term, b: &Term) {
        let a_class = self.get_class(a);
        let a_reduced = self.reduce(a);
        let a_node = self.get_node(a_reduced);
        let b_class = self.get_class(b);
        let b_reduced = self.reduce(b);
        let b_node = self.get_node(b_reduced);
        // println!("({a_class}, {b_class}) -> ({}, {})", a.clone(), b.clone());
        self.eqs.insert((a_class, b_class), (a.clone(), b.clone()));
        // println!("R({a_class}, {b_node}) -> ({}, {})", a.clone(), b.clone());
        // self.eqs_raw
        //     .insert((a_class, b_node), (a.clone(), b.clone()));
        self.merge(a_node, b_node)
    }

    fn has_eq(&mut self, a: &Term, b: &Term) -> bool {
        self.get_class(a) == self.get_class(b)
    }

    fn build_resolvent(&mut self, a: &Term, b: &Term) -> Vec<(Term, Term)> {
        println!("resolving {a} == {b}");
        let a_reduced = self.reduce(a);
        let a_node = self.mapping.get(&a_reduced).unwrap();
        let a_path = self.get_path_from_root(*a_node);
        let b_reduced = self.reduce(b);
        let b_node = self.mapping.get(&b_reduced).unwrap();
        let b_path = self.get_path_from_root(*b_node);
        let a_iter = a_path.iter();
        let a_iter = a_iter.zip_longest(a_path.iter().skip(1));
        let b_iter = b_path.iter();
        let b_iter = b_iter.zip_longest(b_path.iter().skip(1));
        let mut ret = Vec::<(Term, Term)>::new();
        a_iter.zip_longest(b_iter).for_each(|e| match e {
            Both(l, r) => {
                if l != r {
                    match l {
                        Both(l, r) => ret.push(self.get_eq((*r, *l))),
                        _ => (),
                    };
                    match r {
                        Both(l, r) => ret.push(self.get_eq((*r, *l))),
                        _ => (),
                    };
                };
            }
            Left(x) | Right(x) => match x {
                Both(l, r) => ret.push(self.get_eq((*r, *l))),
                _ => (),
            },
        });
        return ret;
    }
    // fn build_resolvent_raw(&mut self, a: &Term, b: &Term) -> Vec<(Term, Term)> {
    //     println!("resolving {a} == {b}");
    //     let a_reduced = self.reduce(a);
    //     let a_node = self.mapping.get(&a_reduced).unwrap();
    //     let a_path = self.get_raw_path_from_root(*a_node);
    //     let b_reduced = self.reduce(b);
    //     let b_node = self.mapping.get(&b_reduced).unwrap();
    //     let b_path = self.get_raw_path_from_root(*b_node);
    //     let a_iter = a_path.iter();
    //     let a_iter = a_iter.zip_longest(a_path.iter().skip(1));
    //     let b_iter = b_path.iter();
    //     let b_iter = b_iter.zip_longest(b_path.iter().skip(1));
    //     let mut ret = Vec::<(Term, Term)>::new();
    //     a_iter.zip_longest(b_iter).for_each(|e| match e {
    //         Both(l, r) => {
    //             if l != r {
    //                 match l {
    //                     Both(l, r) => ret.push(self.get_eq_raw((*r, *l))),
    //                     _ => (),
    //                 };
    //                 match r {
    //                     Both(l, r) => ret.push(self.get_eq_raw((*r, *l))),
    //                     _ => (),
    //                 };
    //             };
    //         }
    //         Left(x) | Right(x) => match x {
    //             Both(l, r) => ret.push(self.get_eq_raw((*r, *l))),
    //             _ => (),
    //         },
    //     });
    //     return ret;
    // }

    fn get_eq(&mut self, pair: (usize, usize)) -> (Term, Term) {
        println!("{}, {}", pair.0, pair.1);
        self.eqs.remove_by_left(&pair).unwrap().1
    }

    // fn get_eq_raw(&mut self, pair: (usize, usize)) -> (Term, Term) {
    //     println!("{}, {}", pair.0, pair.1);
    //     self.eqs_raw.remove_by_left(&pair).unwrap().1
    // }

    fn get_class(&mut self, term: &Term) -> usize {
        let reduced = self.reduce(term);
        let node = self.get_node(reduced);
        self.find_root(node)
    }

    fn get_node(&mut self, reduced: TermClass) -> usize {
        if let Some(eq_class) = self.mapping.get(&reduced) {
            *eq_class
        } else {
            self.new_class(reduced)
        }
    }

    fn reduce(&mut self, term: &Term) -> TermClass {
        match term {
            Term::Identifier(name) => TermClass::Identifier(name.clone()),
            Term::Function(name, terms) => TermClass::Function(
                name.clone(),
                terms
                    .iter()
                    .map(|t| {
                        TermClass::Eq({
                            let node = self.get_class(t);
                            self.find_root(node)
                        })
                    })
                    .collect(),
            ),
        }
    }

    fn new_class(&mut self, term: TermClass) -> usize {
        let class = self.relations.len();
        self.relations.push(class);
        // self.relations_raw.push(class);
        self.mapping.insert(term, class);
        return class;
    }

    fn merge(&mut self, a_node: usize, b_node: usize) {
        let a_root = self.find_root(a_node);
        let b_root = self.find_root(b_node);
        *self.relations.get_mut(a_root).unwrap() = b_root;
        // *self.relations_raw.get_mut(a_root).unwrap() = b_node;
    }

    fn find_root(&self, node: usize) -> usize {
        let parent = *self.relations.get(node).unwrap();
        if parent == node {
            node
        } else {
            self.find_root(parent)
        }
    }

    fn get_path_from_root(&self, node: usize) -> Vec<usize> {
        let parent = *self.relations.get(node).unwrap();
        if parent == node {
            return vec![node];
        } else {
            let mut prefix = self.get_path_from_root(parent);
            prefix.push(node);
            return prefix;
        }
    }

    // fn get_raw_path_from_root(&self, node: usize) -> Vec<usize> {
    //     let parent = *self.relations.get(node).unwrap();
    //     if parent == node {
    //         return vec![node];
    //     } else {
    //         let parent = *self.relations_raw.get(node).unwrap();
    //         if parent == node {
    //             return vec![node];
    //         }
    //         let mut prefix = self.get_raw_path_from_root(parent);
    //         prefix.push(node);
    //         return prefix;
    //     }
    // }
}

impl Display for EquivalenceClasses {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "Mapping:")?;
        self.mapping
            .iter()
            .sorted_by_key(|x| x.1.clone())
            .try_for_each(|(k, v)| {
                write!(f, "{k}")?;
                self.get_path_from_root(*v)
                    .iter()
                    .rev()
                    .try_for_each(|v| write!(f, " -> {v}"))?;
                writeln!(f)
            }) //?;
               // writeln!(f, "Raw mapping:")?;
               // self.mapping
               //     .iter()
               //     .sorted_by_key(|x| x.1.clone())
               //     .try_for_each(|(k, v)| {
               //         write!(f, "{k}")?;
               //         self.get_raw_path_from_root(*v)
               //             .iter()
               //             .rev()
               //             .try_for_each(|v| write!(f, " -> {v}"))?;
               //         writeln!(f)
               //     })
    }
}

#[derive(Eq, Hash, PartialEq, Debug, PartialOrd, Ord)]
enum TermClass {
    Identifier(String),
    Function(String, Vec<TermClass>),
    Eq(usize),
}

impl Display for TermClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TermClass::Identifier(name) => write!(f, "{name}"),
            TermClass::Function(name, terms) => {
                write!(f, "{name}(")?;
                let mut first = true;
                terms.iter().try_for_each(|term| {
                    if !first {
                        write!(f, ", ")?
                    }
                    first = false;
                    write!(f, "{term}")
                })?;
                write!(f, ")")
            }
            TermClass::Eq(class) => write!(f, "[{class}]"),
        }
    }
}

pub mod parser {
    use crate::language::Conjunct;
    use crate::smt_euf::State;
    use nom::combinator::cut;
    use nom::combinator::opt;

    use nom::bytes::complete::tag;

    use nom::character::complete::{alpha1, alphanumeric0, char, digit1, multispace0};

    use nom::combinator::eof;
    use nom::combinator::map;
    use nom::combinator::recognize;
    use nom::error::context;
    use nom::error::ParseError;

    use nom::sequence::tuple;
    use nom::sequence::{pair, terminated};

    use nom::branch::alt;
    use nom::multi::separated_list1;
    use nom::sequence::{delimited, separated_pair};
    use nom::IResult;

    use crate::language::Disjunct;

    use super::{Atom, Term};

    pub fn formula<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Conjunct<Disjunct<Atom, State>, State>, E> {
        cnf(input)
    }

    fn cnf<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Conjunct<Disjunct<Atom, State>, State>, E> {
        delimited(
            multispace0,
            alt((
                context(
                    "cnf",
                    map(
                        terminated(
                            separated_list1(
                                delimited(multispace0, tag("&&"), multispace0),
                                disjunct,
                            ),
                            eof,
                        ),
                        |disjuncts| Conjunct::new(disjuncts),
                    ),
                ),
                context(
                    "cnf in parentheses",
                    terminated(delimited(char('('), cnf, char(')')), eof),
                ),
            )),
            multispace0,
        )(input)
    }

    fn disjunct<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Disjunct<Atom, State>, E> {
        delimited(
            multispace0,
            alt((
                context(
                    "disjunct",
                    map(
                        separated_list1(delimited(multispace0, tag("||"), multispace0), atom),
                        |atoms| Disjunct::new(atoms),
                    ),
                ),
                context(
                    "disjunct in parentheses",
                    delimited(char('('), disjunct, char(')')),
                ),
            )),
            multispace0,
        )(input)
    }

    fn atom<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Atom, E> {
        alt((
            context("atom in parentheses", delimited(char('('), atom, char(')'))),
            context(
                "eq atom",
                map(
                    separated_pair(term, delimited(multispace0, tag("=="), multispace0), term),
                    |(a, b)| Atom::Eq(a, b),
                ),
            ),
            context(
                "neq atom",
                map(
                    separated_pair(term, delimited(multispace0, tag("!="), multispace0), term),
                    |(a, b)| Atom::Neq(a, b),
                ),
            ),
        ))(input)
    }

    fn term<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Term, E> {
        alt((
            context("term in parentheses", delimited(char('('), term, char(')'))),
            context(
                "function term",
                map(term_function, |(name, terms)| Term::Function(name, terms)),
            ),
            context(
                "identifier term",
                map(identifier, |name| Term::Identifier(name)),
            ),
            context(
                "constant term",
                map(constant, |name| Term::Identifier(name)),
            ),
        ))(input)
    }

    fn term_function<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, (String, Vec<Term>), E> {
        pair(
            identifier,
            delimited(
                char('('),
                cut(separated_list1(
                    delimited(multispace0, char(','), multispace0),
                    term,
                )),
                cut(char(')')),
            ),
        )(input)
    }

    fn identifier<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, String, E> {
        map(recognize(tuple((alpha1, alphanumeric0))), |s: &str| {
            s.into()
        })(input)
    }

    fn constant<'a, E: ParseError<&'a str> + nom::error::ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, String, E> {
        map(
            recognize(pair(digit1, opt(pair(char('.'), digit1)))),
            |s: &str| s.into(),
        )(input)
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use nom::error::VerboseError;
        use nom::Finish;

        #[test]
        fn test_formula_parser() -> Result<(), ()> {
            macro_rules! testcase {
                ($source:literal, $representation:literal) => {
                    match formula::<VerboseError<&str>>($source).finish() {
                        Ok((_, formula)) => assert_eq!(format!("{formula}"), $representation),
                        Err(err) => Err(println!(
                            "`Parse error: {}",
                            nom::error::convert_error($source, err)
                        ))?,
                    }
                };
            }
            testcase!("q(1)==x", "((q(1) == x))");
            testcase!("(a == x) || b == c", "((a == x) || (b == c))");
            testcase!(
                "(a(b(c(1, literal))) ==       1 || x == y) && (z == c)",
                "((a(b(c(1, literal))) == 1) || (x == y)) && ((z == c))"
            );
            testcase!(
                "a == b || c ==d && a == x",
                "((a == b) || (c == d)) && ((a == x))"
            );
            testcase!("a != b || c == d", "((a != b) || (c == d))");
            Ok(())
        }
    }
}

pub mod solver {
    use super::*;
    use crate::language::Conjunct;
    use crate::language::Disjunct;
    use crate::Error;
    use std::cmp::min;
    use std::fmt;
    use std::fmt::Display;

    #[derive(Debug, PartialEq)]
    pub enum Decision<Implication> {
        Always,
        Never,
        Implies(Implication),
        Undecided,
    }

    trait Theory<Implication> {
        fn extend(&mut self, implication: &Implication) -> Result<(), Error>;
        fn reduce(&mut self, implication: &Implication) -> Result<(), Error>;
    }

    impl Theory<Atom> for State {
        fn extend(&mut self, implication: &Atom) -> Result<(), Error> {
            // self.atoms.insert(k, v)
            let a = self.atoms.insert(implication.clone(), Value::Some(true));
            let b = self
                .atoms
                .insert(implication.inverted(), Value::Some(false));
            match (a, b) {
                (None, None) => Ok(()),
                _ => Err(Error::DPLLT("duplicate implication".to_owned())),
            }
        }
        fn reduce(&mut self, implication: &Atom) -> Result<(), Error> {
            let a = self.atoms.remove(implication);
            let b = self.atoms.remove(&implication.inverted());
            match (a, b) {
                (Some(..), Some(..)) => Ok(()),
                _ => Err(Error::DPLLT("missing implication".to_owned())),
            }
        }
    }

    trait Analyzable<'a, Theory, Implication, E> {
        fn analyze(&'a self, theory: &Theory) -> Result<Decision<&'a Implication>, E>;
        fn hypothesize(&self, theory: &Theory) -> Option<Implication>;
    }

    impl Analyzable<'_, State, Atom, Error> for Disjunct<Atom, State> {
        fn analyze(&self, state: &State) -> Result<Decision<&Atom>, Error> {
            Ok(self.atoms.iter().fold(Decision::Never, |acc, next| {
                match (acc, next.compute(state)) {
                    (Decision::Always, ..) => Decision::Always,
                    (.., Value::Some(true)) => Decision::Always,
                    (Decision::Never, Value::Any) => Decision::Implies(next),
                    (Decision::Implies(..), Value::Any) => Decision::Undecided,
                    (decision, Value::Some(false)) => decision,
                    (Decision::Undecided, Value::Any) => Decision::Undecided,
                }
            }))
        }
        fn hypothesize(&self, state: &State) -> Option<Atom> {
            for atom in &self.atoms {
                if let Value::Any = atom.compute(state) {
                    return Some(atom.inverted());
                }
            }
            None
        }
    }

    #[derive(Debug)]
    pub enum SolverVerdict<Theory> {
        Satisfiable(Theory),
        NotSatisfiable,
    }

    impl<Theory: Display> Display for SolverVerdict<Theory> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self {
                &SolverVerdict::NotSatisfiable => write!(f, "UNSAT"),
                &SolverVerdict::Satisfiable(theory) => {
                    write!(f, "SAT: {theory}")
                }
            }
        }
    }

    impl Disjunct<Atom, State> {
        fn resolve(&mut self, other: &Disjunct<Atom, State>, atom: &Atom) -> Result<(), Error> {
            let inverted_atom = atom.inverted();
            let filter = |x: &Atom| {
                if x == atom || *x == inverted_atom {
                    return None;
                } else {
                    return Some(x.clone());
                }
            };
            self.atoms = self
                .atoms
                .iter()
                .filter_map(filter)
                .chain(other.atoms.iter().filter_map(filter))
                .collect();
            self.atoms.sort();
            self.atoms.dedup();
            Ok(())
        }
    }

    impl Conjunct<Disjunct<Atom, State>, State> {
        pub fn solve(&self) -> Result<SolverVerdict<State>, Error> {
            let mut state = State::new();
            let mut mut_self = (*self).clone();
            let mut decisions = Vec::<Atom>::new();
            let mut all_implications_in_order = Vec::<Vec<(Atom, Disjunct<Atom, State>)>>::new();
            let mut literal_levels = HashMap::<Atom, usize>::new();
            loop {
                let current_level = decisions.len();
                let mut implications_in_order =
                    if all_implications_in_order.len() == current_level + 1 {
                        all_implications_in_order.pop().unwrap()
                    } else {
                        Vec::<(Atom, Disjunct<Atom, State>)>::new()
                    };
                let mut exhausted = false;
                let mut learned_to_add = None::<Disjunct<Atom, State>>;
                'propagation: while !exhausted {
                    exhausted = true;
                    for disjunct in &mut_self.atoms {
                        match disjunct.analyze(&state)? {
                            Decision::Implies(implication) => {
                                exhausted = false;
                                implications_in_order.push((implication.clone(), disjunct.clone()));
                                literal_levels.insert(implication.clone(), current_level);
                                state.extend(implication)?;
                            }
                            Decision::Never => {
                                if current_level == 0 {
                                    return Ok(SolverVerdict::NotSatisfiable);
                                }
                                let mut learned = disjunct.clone();
                                while let Some((last_implication, disjunct)) =
                                    implications_in_order.pop()
                                {
                                    state.reduce(&last_implication)?;
                                    literal_levels.remove(&last_implication);
                                    if !learned.atoms.contains(&last_implication.inverted()) {
                                        continue;
                                    }
                                    learned.resolve(&disjunct, &last_implication)?;
                                    let amt = implications_in_order.iter().fold(
                                        0,
                                        |acc, (literal, _)| {
                                            if learned.atoms.contains(&literal.inverted())
                                                || learned.atoms.contains(&literal)
                                            {
                                                acc + 1
                                            } else {
                                                acc
                                            }
                                        },
                                    );
                                    let amt = if let Some(literal) = decisions.last() {
                                        if learned.atoms.contains(&literal.inverted())
                                            || learned.atoms.contains(&literal)
                                        {
                                            amt + 1
                                        } else {
                                            amt
                                        }
                                    } else {
                                        amt
                                    };
                                    if amt <= 1 {
                                        break;
                                    }
                                }
                                learned_to_add = Some(learned);
                                break 'propagation;
                            }
                            _ => (),
                        }
                    }
                }
                let mut override_backtrack = false;
                'adjusting: loop {
                    match learned_to_add {
                        Some(learned) => {
                            let backtrack_level = learned
                                .atoms
                                .iter()
                                .filter_map(|from| literal_levels.get(&from.inverted()))
                                .fold((None, None), |acc, next| match acc {
                                    (None, None) => return (Some(*next), None),
                                    (Some(x), None) => (Some(max(x, *next)), Some(min(x, *next))),
                                    (Some(x), Some(y)) => {
                                        (Some(max(x, *next)), Some(max(min(x, *next), y)))
                                    }
                                    x => x,
                                })
                                .1
                                .unwrap_or(0);
                            // println!("backtracking from {current_level} to {backtrack_level}");
                            if backtrack_level == current_level && !override_backtrack {
                                return Ok(SolverVerdict::NotSatisfiable);
                            } else {
                                for (implication, _) in implications_in_order {
                                    literal_levels.remove(&implication);
                                    state.reduce(&implication)?;
                                }
                                while decisions.len() > backtrack_level {
                                    if all_implications_in_order.len() == decisions.len() + 1 {
                                        for (implication, _) in
                                            all_implications_in_order.pop().unwrap()
                                        {
                                            literal_levels.remove(&implication);
                                            state.reduce(&implication)?;
                                        }
                                    }
                                    let decision = decisions.pop().unwrap();
                                    literal_levels.remove(&decision);
                                    state.reduce(&decision)?;
                                }
                            }
                            mut_self.atoms.push(learned);
                            break;
                        }
                        None => {
                            if state.is_correct() {
                                for disjunct in &mut_self.atoms {
                                    if let Some(hypothesis) = disjunct.hypothesize(&state) {
                                        decisions.push(hypothesis.clone());
                                        literal_levels
                                            .insert(hypothesis.clone(), current_level + 1);
                                        all_implications_in_order.push(implications_in_order);
                                        state.extend(&hypothesis)?;
                                        break 'adjusting;
                                    }
                                }
                                return Ok(SolverVerdict::Satisfiable(state));
                            }
                            learned_to_add = Some(Disjunct::new(
                                state.as_atoms().iter().map(|v| v.inverted()).collect(),
                            ));
                            // println!("{current_level}");
                            // println!("{state}");
                            // println!("learned {}", learned_to_add.clone().unwrap());
                            override_backtrack = true;
                        }
                    }
                }
            }
        }
    }
}
