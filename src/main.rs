use std::collections::{HashMap, HashSet};
use std::collections::hash_map::RandomState;
use std::error::Error;
use std::fmt::{Display, Formatter, Write};
use std::hash::Hash;
use std::io::stdin;
use std::iter::Peekable;
use std::ops::Not;

#[allow(unused_imports)]
use inline_colorization::color_reset;
use itertools::Itertools;
use rand::{Rng, thread_rng};

use crate::ParseError::{ExpectedCloseParent, ExpectedExpr, ReachedEOF, UnexpectedToken};
use crate::TokenizingError::UnexpectedChar;

const MACRON: char = '\u{0304}';

fn main() -> anyhow::Result<()> {
    println!("Enter expression");
    let line = read_line()?;
    let tokens = tokenize(&line)?;
    let mut parser = Parser::new(tokens.into_iter());
    let expr = parser.expr()?;
    println!("FN = {}", expr);
    print_truth_table(&expr);
    println!();
    Ok(())
}

fn read_line() -> anyhow::Result<String> {
    let mut buf = String::new();
    let _ = stdin().read_line(&mut buf)?;
    Ok(buf.trim_end_matches(|c| c == '\n').to_string())
}


fn print_truth_table(expr: &Op) {
    let vars = Vars::new(expr.variables().into_iter().sorted().collect());
    let colors = generate_colors(vars.names());
    println!("{}FN", vars.names().iter().flat_map(|c| [colorize(*c, colors.get(c).unwrap()), "\t".to_string()]).collect::<String>());
    vars.for_each(|state| {
        print!("{}", state.keys().sorted().flat_map(|b| [bool_digit(*state.get(b).expect("The key is gotten from this map. This cannot fail")), '\t']).collect::<String>());
        let colorized_substituted_expr = expr.to_string().chars().map(|c| {
            if let Some(&bool) = state.get(&c) {
                if let Some(color) = colors.get(&c) {
                    colorize(bool_digit(bool), color)
                } else {
                    bool_digit(bool).to_string()
                }
            } else {
                c.to_string()
            }
        }).collect::<String>();
        println!("{sub} = {res}",
                 sub = colorized_substituted_expr,
                 res = bool_digit(expr.eval(&state).unwrap()));
    });
}

fn colorize<T>(t: T, color: Color) -> String where T: Display {
    format!("{color}{t}{color_reset}")
}

type Color = &'static str;

const COLORS: [Color; 14] = {
    use inline_colorization::*;
    [color_black, color_blue, color_cyan, color_green, color_magenta, color_red, color_yellow,
        color_bright_black, color_bright_blue, color_bright_cyan, color_bright_green, color_bright_magenta, color_bright_red, color_bright_yellow]
};

fn generate_colors(names: &[char]) -> HashMap<char, Color> {
    names.iter().copied().map(|c| (c, COLORS[thread_rng().gen_range(0..14)])).collect()
}

fn tokenize(input: &str) -> Result<Vec<Token>, TokenizingError> {
    use Token::*;
    input.chars().enumerate().map(|(index, c)| match c {
        '1' | '0' => Ok(Some(Const(c == '1'))),
        '!' => Ok(Some(Bang)),
        '+' => Ok(Some(Plus)),
        '*' => Ok(Some(Star)),
        '^' => Ok(Some(Caret)),
        '(' => Ok(Some(LParen)),
        ')' => Ok(Some(RParen)),
        v if v.is_alphabetic() => Ok(Some(Var(v.to_ascii_uppercase()))),
        ws if ws.is_whitespace() => Ok(None),
        _ => Err(UnexpectedChar(c, index))
    }).collect::<Result<Vec<_>, TokenizingError>>().map(|v| v.into_iter().flatten().collect())
}

fn moving_union<T: Hash + Eq>(a: HashSet<T>, b: HashSet<T>) -> HashSet<T, RandomState> {
    let mut s = HashSet::new();
    s.extend(a.into_iter());
    s.extend(b.into_iter());
    s
}

fn bool_digit(b: bool) -> char {
    if b { '1' } else { '0' }
}


#[derive(Debug)]
enum ParseError {
    ReachedEOF,
    ExpectedCloseParent,
    UnexpectedToken(Token),
    ExpectedExpr,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}

impl Error for ParseError {}


#[derive(Debug)]
enum TokenizingError {
    UnexpectedChar(char, usize)
}

impl Display for TokenizingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnexpectedChar(char, index) => f.write_fmt(format_args!("Unexpected character `{char}` at index {index}"))
        }
    }
}

impl Error for TokenizingError {}

struct Vars {
    names: Vec<char>,
    current_pos: usize,
}

impl Vars {
    fn new(names: Vec<char>) -> Self {
        Self {
            names,
            current_pos: 0,
        }
    }

    fn names(&self) -> &Vec<char> {
        &self.names
    }
}

impl Iterator for Vars {
    type Item = HashMap<char, bool>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_pos >= 2usize.pow(self.names.len() as u32) {
            None
        } else {
            let bin = format!("{pos:0>n$b}", pos = self.current_pos, n = self.names.len());
            let res = HashMap::from_iter(
                self.names.iter().zip(bin.chars()).map(|(&name, val)| (name, val == '1'))
            );
            self.current_pos += 1;
            Some(res)
        }
    }
}


struct Parser<I: Iterator<Item=Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item=Token>> Parser<I> {
    fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn expr(&mut self) -> Result<Op, ParseError> {
        self.or()
    }

    fn term(&mut self) -> Result<Op, ParseError> {
        use Token::*;
        match self.tokens.next().ok_or(ReachedEOF)? {
            Const(b) => Ok(Op::Const(b)),
            Var(c) => Ok(Op::Var(c)),
            LParen => {
                let expr = self.expr();
                self.tokens.next_if(|t| matches!(t, Token::RParen)).ok_or(ExpectedCloseParent).and(expr)
            }
            t => Err(UnexpectedToken(t))
        }
    }

    fn or(&mut self) -> Result<Op, ParseError> {
        use Op::*;
        let mut lhs = self.and()?;
        while self.tokens.next_if(|t| matches!(t, Token::Plus)).is_some() {
            let rhs = self.and()?;
            lhs = match (lhs, rhs) {
                (Var(v1), Var(v2)) if v1 == v2 => Var(v1),
                (Const(true), _) | (_, Const(true)) => Const(true),
                (Const(false), op) | (op, Const(false)) => op,
                (lhs, rhs) => Or(Box::new(lhs), Box::new(rhs))
            };
        }
        Ok(lhs)
    }

    fn and(&mut self) -> Result<Op, ParseError> {
        use Op::*;
        let mut lhs = self.xor()?;
        while self.tokens.next_if(|t| matches!(t, Token::Star)).is_some() {
            let rhs = self.xor()?;
            lhs = match (lhs, rhs) {
                (Var(v1), Var(v2)) if v1 == v2 => Var(v1),
                (Const(true), op) | (op, Const(true)) => op,
                (Const(false), _) | (_, Const(false)) => Const(false),
                (lhs, rhs) => And(Box::new(lhs), Box::new(rhs)),
            };
        }
        Ok(lhs)
    }

    fn xor(&mut self) -> Result<Op, ParseError> {
        use Op::*;
        let mut lhs = self.not()?;
        while self.tokens.next_if(|t| matches!(t, Token::Caret)).is_some() {
            let rhs = self.not()?;
            lhs = match (lhs, rhs) {
                (Var(v1), Var(v2)) if v1 == v2 => Const(false),
                (lhs, rhs) => Xor(Box::new(lhs), Box::new(rhs))
            };
        }
        Ok(lhs)
    }

    fn not(&mut self) -> Result<Op, ParseError> {
        if self.tokens.next_if(|t| matches!(t, Token::Bang)).is_some() {
            let expr = self.not().map_err(|_| ExpectedExpr)?;
            match expr {
                Op::Const(b) => Ok(Op::Const(!b)),
                Op::Not(op) => Ok(*op),
                _ => Ok(Op::Not(Box::new(expr)))
            }
        } else {
            self.term()
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Token {
    Bang,
    Plus,
    Star,
    Var(char),
    LParen,
    RParen,
    Const(bool),
    Caret,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Op {
    Const(bool),
    Var(char),
    Not(Box<Op>),
    Or(Box<Op>, Box<Op>),
    Xor(Box<Op>, Box<Op>),
    And(Box<Op>, Box<Op>),
}

impl Op {
    fn variables(&self) -> HashSet<char> {
        match self {
            Op::Const(_) => HashSet::new(),
            Op::Var(name) => HashSet::from([*name]),
            Op::Not(op) => op.variables(),
            Op::Or(a, b) | Op::And(a, b) | Op::Xor(a, b) => moving_union(a.variables(), b.variables())
        }
    }

    fn eval(&self, vars: &HashMap<char, bool>) -> Option<bool> {
        match self {
            Op::Const(b) => Some(*b),
            Op::Var(c) => vars.get(c).copied(),
            Op::Not(op) => op.eval(vars).map(bool::not),
            Op::Or(a, b) => Some((a.eval(vars)?) || (b.eval(vars)?)),
            Op::And(a, b) => Some((a.eval(vars)?) && (b.eval(vars)?)),
            Op::Xor(a, b) => Some((a.eval(vars)?) ^ (b.eval(vars)?)),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Const(b) => f.write_char(bool_digit(*b)),
            Op::Var(name) => f.write_fmt(format_args!("{name}")),
            Op::Not(op) => {
                if let Op::Var(name) = **op {
                    f.write_fmt(format_args!("{name}{MACRON}"))
                } else {
                    f.write_fmt(format_args!("!{op}"))
                }
            }
            Op::Or(lhs, rhs) => f.write_fmt(format_args!("({lhs} + {rhs})")),
            Op::And(lhs, rhs) => f.write_fmt(format_args!("({lhs} * {rhs})")),
            Op::Xor(lhs, rhs) => f.write_fmt(format_args!("({lhs} ^ {rhs})")),
        }
    }
}