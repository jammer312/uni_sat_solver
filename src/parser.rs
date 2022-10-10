use crate::language::Disjunct;
use crate::language::CNF;
use nom::bytes::complete::tag;
use nom::character::complete::digit0;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;
use nom::character::complete::one_of;
use nom::character::complete::{char, line_ending, multispace1, not_line_ending};
use nom::combinator::map;
use nom::combinator::map_res;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::combinator::value;
use nom::error::VerboseError;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::pair;

use nom::sequence::preceded;
use nom::sequence::separated_pair;
use nom::sequence::terminated;

use nom::sequence::tuple;
use nom::IResult;

fn comment(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    value(
        (),
        tuple((
            multispace0,
            char('c'),
            multispace1,
            not_line_ending,
            line_ending,
        )),
    )(input)
}

fn comment0(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    value((), pair(multispace0, opt(comment)))(input)
}

fn comment1(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    value((), pair(multispace1, opt(comment)))(input)
}

fn udigit(input: &str) -> IResult<&str, usize, VerboseError<&str>> {
    map_res(preceded(comment0, digit1), |out: &str| out.parse())(input)
}

fn sdigit_nz(input: &str) -> IResult<&str, isize, VerboseError<&str>> {
    map_res(
        preceded(
            comment0,
            recognize(tuple((opt(one_of("-+")), one_of("123456789"), digit0))),
        ),
        |out: &str| out.parse(),
    )(input)
}

fn header(input: &str) -> IResult<&str, (usize, usize), VerboseError<&str>> {
    preceded(
        tuple((comment0, char('p'), comment1, tag("cnf"))),
        separated_pair(udigit, comment1, udigit),
    )(input)
}

fn clause(input: &str) -> IResult<&str, Disjunct, VerboseError<&str>> {
    map_res(
        terminated(many1(sdigit_nz), pair(comment0, char('0'))),
        |v| Ok::<Disjunct, ()>(Disjunct::new(v)),
    )(input)
}

pub fn cnf(input: &str) -> IResult<&str, Result<CNF, crate::Error>, VerboseError<&str>> {
    map(pair(opt(header), many0(clause)), |res| {
        CNF::new(res.0, res.1)
    })(input)
}
