use std::fmt;
use std::fmt::Display;

use crate::language::Disjunct;
use crate::language::Literal;
use crate::language::State;
use crate::language::Value;
use crate::language::CNF;
use crate::Error;

#[derive(Debug, PartialEq)]
pub enum Decision<Implication> {
    Always,
    Never,
    Implies(Implication),
    Undecided,
}

trait Theory<Implication> {
    fn extend(&mut self, extender: &Implication) -> Result<(), Error>;
    fn reduce(&mut self, reducer: &Implication);
}

impl Theory<Literal> for State {
    fn extend(&mut self, implication: &Literal) -> Result<(), Error> {
        self.add(implication)
    }
    fn reduce(&mut self, implication: &Literal) {
        self.remove(implication)
    }
}

trait Analyzable<'a, Theory, Implication, E> {
    fn analyze(&'a self, theory: &Theory) -> Result<Decision<&'a Implication>, E>;
    fn hypothesize(&self, theory: &Theory) -> Option<Implication>;
}

impl Analyzable<'_, State, Literal, Error> for Disjunct {
    fn analyze(&self, state: &State) -> Result<Decision<&Literal>, Error> {
        Ok(self.literals.iter().fold(Decision::Never, |acc, next| {
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
    fn hypothesize(&self, state: &State) -> Option<Literal> {
        for literal in &self.literals {
            if let Value::Any = literal.compute(state) {
                return Some(literal.inverted());
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Error;

    #[test]
    fn test_disjunct_analyze() -> Result<(), Error> {
        let disjunct = Disjunct::new(vec![1, -2, 3]);
        macro_rules! stateFrom {
        [$($x:literal),*] => {
            &State::new(vec![$($x),*])?
            }
        }
        macro_rules! testcase {
            ([$($x:literal),*], $expected:ident) => {
                assert_eq!(disjunct.analyze(stateFrom![$($x),*])?, Decision::$expected);
            };
            ([$($x:literal),*], $expected:ident($implies:expr)) => {
                assert_eq!(disjunct.analyze(stateFrom![$($x),*])?, Decision::$expected($implies));
            };
        }
        testcase!([1], Always);
        testcase!([-2], Always);
        testcase!([3], Always);
        testcase!([-1, -2], Always);
        testcase!([-1, 2, -3], Never);
        testcase!([-1, 2], Implies(&Literal::new(&3)));
        testcase!([-1, -3], Implies(&Literal::new(&-2)));
        testcase!([], Undecided);
        testcase!([-1], Undecided);
        testcase!([2], Undecided);
        testcase!([-3], Undecided);
        Ok(())
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

pub trait Solvable<Theory, E> {
    fn solve(&self) -> Result<SolverVerdict<Theory>, E>;
}

mod cdcl {

    use std::cmp::max;
    use std::cmp::min;
    use std::collections::HashMap;

    use super::*;

    impl Disjunct {
        fn resolve(&mut self, other: &Disjunct, literal: &Literal) -> Result<(), Error> {
            let filter = |x: &Literal| {
                if x.index == literal.index {
                    return None;
                } else {
                    return Some(x.clone());
                }
            };
            self.literals = self
                .literals
                .iter()
                .filter_map(filter)
                .chain(other.literals.iter().filter_map(filter))
                .collect();
            self.literals.sort();
            self.literals.dedup();
            Ok(())
        }
    }

    impl Solvable<State, Error> for CNF {
        fn solve(&self) -> Result<SolverVerdict<State>, Error> {
            let mut state = State::new_empty(self.width());
            let mut mut_self = (*self).clone();
            let mut decisions = Vec::<Literal>::new();
            let mut all_implications_in_order = Vec::<Vec<(Literal, Disjunct)>>::new();
            let mut levels = HashMap::<Literal, usize>::new();
            let mut last_learned = None;
            loop {
                // println!("level: {}", decisions.len());
                // println!("current CNF: {mut_self}");
                // println!("current state: {state}");
                let current_level = decisions.len();
                let mut implications_in_order =
                    if all_implications_in_order.len() == current_level + 1 {
                        all_implications_in_order.pop().unwrap()
                    } else {
                        Vec::<(Literal, Disjunct)>::new()
                    };
                let mut exhausted = false;
                let mut learned_to_add = None::<Disjunct>;
                'propagation: while !exhausted {
                    exhausted = true;
                    for disjunct in &mut_self.disjuncts {
                        match disjunct.analyze(&state)? {
                            Decision::Implies(implication) => {
                                exhausted = false;
                                implications_in_order.push((implication.clone(), disjunct.clone()));
                                levels.insert(*implication, current_level);
                                state.extend(implication)?;
                            }
                            Decision::Never => {
                                if current_level == 0 {
                                    return Ok(SolverVerdict::NotSatisfiable);
                                }
                                let mut learned = disjunct.clone();
                                // println!("conflict: {learned}");
                                while let Some((last_implication, disjunct)) =
                                    implications_in_order.pop()
                                {
                                    // print!("{last_implication} deduced from {disjunct}");
                                    state.reduce(&last_implication);
                                    levels.remove(&last_implication);
                                    if !learned.literals.contains(&last_implication.inverted()) {
                                        // println!(" not related");
                                        continue;
                                    }
                                    // println!(" related");
                                    learned.resolve(&disjunct, &last_implication)?;
                                    // println!("after resolve: {learned}");
                                    let amt = implications_in_order.iter().fold(
                                        0,
                                        |acc, (literal, _)| {
                                            if learned.literals.contains(&literal.inverted())
                                                || learned.literals.contains(&literal)
                                            {
                                                acc + 1
                                            } else {
                                                acc
                                            }
                                        },
                                    );
                                    let amt = if let Some(literal) = decisions.last() {
                                        if learned.literals.contains(&literal.inverted())
                                            || learned.literals.contains(&literal)
                                        {
                                            amt + 1
                                        } else {
                                            amt
                                        }
                                    } else {
                                        amt
                                    };
                                    if amt <= 1 {
                                        // println!("amt: {amt}, breaking prematurely");
                                        break;
                                    }
                                    // println!("amt:  {amt}");
                                }
                                learned_to_add = Some(learned);
                                break 'propagation;
                            }
                            _ => (),
                        }
                    }
                }
                if let Some(learned) = learned_to_add {
                    if let Some(last_learned) = last_learned {
                        if last_learned == learned {
                            return Err(Error::SolverError("learned the same".to_owned()));
                        }
                    }
                    last_learned = Some(learned.clone());
                    // let mut literals: Vec<&Literal> = levels.keys().collect();
                    // literals.sort();
                    // literals.iter().for_each(|key| {
                    //     println!("{key}: {}", levels.get(key).unwrap());
                    // });
                    // println!("------");
                    // println!("{learned}");
                    let backtrack_level = learned
                        .literals
                        .iter()
                        .filter_map(|from| levels.get(&from.inverted()))
                        .fold((None, None), |acc, next| {
                            // println!("{next}");
                            match acc {
                                (None, None) => return (Some(*next), None),
                                (Some(x), None) => (Some(max(x, *next)), Some(min(x, *next))),
                                (Some(x), Some(y)) => {
                                    (Some(max(x, *next)), Some(max(min(x, *next), y)))
                                }
                                x => x,
                            }
                        })
                        .1
                        .unwrap_or(0);
                    if backtrack_level == current_level {
                        return Ok(SolverVerdict::NotSatisfiable);
                    } else {
                        // println!("conflict, learned {learned}, rolled back to {backtrack_level}",);
                        for (implication, _) in implications_in_order {
                            levels.remove(&implication);
                            state.reduce(&implication);
                        }
                        while decisions.len() > backtrack_level {
                            if all_implications_in_order.len() == decisions.len() + 1 {
                                for (implication, _) in all_implications_in_order.pop().unwrap() {
                                    levels.remove(&implication);
                                    state.reduce(&implication);
                                }
                            }
                            let decision = decisions.pop().unwrap();
                            levels.remove(&decision);
                            state.reduce(&decision);
                        }
                    }
                    mut_self.disjuncts.push(learned);
                } else {
                    // println!("resulting state: {state}");
                    // println!("no conflict");
                    if state.is_filled() {
                        return Ok(SolverVerdict::Satisfiable(state));
                    }
                    for disjunct in &mut_self.disjuncts {
                        if let Some(hypothesis) = disjunct.hypothesize(&state) {
                            decisions.push(hypothesis);
                            levels.insert(hypothesis, current_level + 1);
                            all_implications_in_order.push(implications_in_order);
                            state.extend(&hypothesis)?;
                            // println!("hypothesis: {hypothesis}");
                            break;
                        }
                    }
                }
            }
        }
    }
}
