use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter, Write};
use std::hash::Hash;
use std::io::{stdin, stdout, IsTerminal};
use std::iter::Peekable;
use std::ops::Not;
use itertools::Itertools;

use crate::ParseError::{ExpectedCloseParent, ReachedEOF, UnexpectedToken, UnparsedToken};
use crate::TokenizingError::UnexpectedChar;

macro_rules! negate {
    ($lhs:ident) => {
        $lhs = Op::Not(Box::new($lhs))
    };
}

fn main() -> anyhow::Result<()> {
    eprintln!("Enter expression");
    let line = read_line()?;
    let tokens = tokenize(&line)?;
    let mut parser = Parser::new(tokens.into_iter());
    let expr = parser.expr()?;
    eprintln!("FN = {expr}");
    print_truth_table(&expr);
    Ok(())
}

fn read_line() -> anyhow::Result<String> {
    let mut buf = String::new();
    let _ = stdin().read_line(&mut buf)?;
    Ok(buf.trim_end_matches('\n').to_string())
}

fn print_truth_table(expr: &Op) {
    let vars = Vars::new(expr.variables().into_iter().sorted().collect());
    let colors = generate_colors(vars.names());
    println!(
        "{}FN",
        vars.names()
            .iter()
            .flat_map(|c| [colorize(*c, colors.get(c).unwrap()), "\t".to_string()])
            .collect::<String>()
    );
    vars.for_each(|state| {
        print!(
            "{}",
            state
                .iter()
                .sorted_by_key(|(k, _)| **k)
                .flat_map(|(_, v)| [bool_digit(*v), '\t'])
                .collect::<String>()
        );
        let colorized_substituted_expr = expr
            .to_string()
            .chars()
            .map(|c| {
                if let Some(&bool) = state.get(&c) {
                    if let Some(color) = colors.get(&c) {
                        colorize(bool_digit(bool), color)
                    } else {
                        bool_digit(bool).to_string()
                    }
                } else {
                    c.to_string()
                }
            })
            .collect::<String>();
        println!(
            "{sub} = {res}",
            sub = colorized_substituted_expr,
            res = bool_digit(expr.eval(&state).unwrap())
        );
    });
}

fn colorize<T>(t: T, color: Color) -> String
where
    T: Display,
{
    use inline_colorization::color_reset;
    if stdout().is_terminal() {
        format!("{color}{t}{color_reset}")
    } else {
        t.to_string()
    }
}

type Color = &'static str;

const COLORS: [Color; 12] = {
    #[allow(clippy::wildcard_imports)]
    use inline_colorization::*;
    [
        color_red,
        color_yellow,
        color_green,
        color_cyan,
        color_blue,
        color_magenta,
        color_bright_red,
        color_bright_yellow,
        color_bright_green,
        color_bright_cyan,
        color_bright_blue,
        color_bright_magenta,
    ]
};

fn generate_colors(names: &[char]) -> HashMap<char, Color> {
    names
        .iter()
        .copied()
        .zip(COLORS.into_iter().cycle())
        .collect()
}

fn tokenize(input: &str) -> Result<Vec<Token>, TokenizingError> {
    let mut chars = input.chars().enumerate().peekable();
    let mut tokens = Vec::new();
    while let Some((index, ch)) = chars.next() {
        tokens.push(match ch {
            '1' | '0' => Token::Const(ch == '1'),
            '+' => Token::Plus,
            '*' => Token::Star,
            '^' => Token::Caret,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '!' => {
                match chars
                    .next_if(|(_, c)| matches!(c, '+' | '*' | '^'))
                    .map(|(_, c)| c)
                {
                    Some('+') => Token::BPlus,
                    Some('*') => Token::BStar,
                    Some('^') => Token::BCaret,
                    _ => Token::Bang,
                }
            }
            v if v.is_alphabetic() => Token::Var(v.to_ascii_uppercase()),
            ws if ws.is_whitespace() => continue,
            _ => return Err(UnexpectedChar(ch, index)),
        });
    }
    Ok(tokens)
}

fn combine_sort_unique<T: Ord + Clone + Hash>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    let mut res = Vec::new();
    res.extend(a);
    res.extend(b);
    res.into_iter().unique().sorted().collect()
}

fn bool_digit(b: bool) -> char {
    if b {
        '1'
    } else {
        '0'
    }
}

#[derive(Debug)]
enum ParseError {
    ReachedEOF,
    ExpectedCloseParent,
    UnexpectedToken,
    UnparsedToken,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}

impl Error for ParseError {}

#[derive(Debug)]
enum TokenizingError {
    UnexpectedChar(char, usize),
}

impl Display for TokenizingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnexpectedChar(char, index) => f.write_fmt(format_args!(
                "Unexpected character `{char}` at index {index}"
            )),
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
        if self.current_pos >= 2usize.pow(u32::try_from(self.names.len()).unwrap_or(u32::MAX)) {
            None
        } else {
            let bin = format!("{pos:0>n$b}", pos = self.current_pos, n = self.names.len());
            let res = self
                .names
                .iter()
                .zip(bin.chars())
                .map(|(&name, val)| (name, val == '1'))
                .collect::<HashMap<_, _>>();
            self.current_pos += 1;
            Some(res)
        }
    }
}

struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn expr(&mut self) -> Result<Op, ParseError> {
        let expr = self.or();
        if self.tokens.peek().is_some() {
            Err(UnparsedToken)
        } else {
            expr
        }
    }

    fn term(&mut self) -> Result<Op, ParseError> {
        use Token::{Const, LParen, RParen, Var};
        match self.tokens.next().ok_or(ReachedEOF)? {
            Const(b) => Ok(Op::Const(b)),
            Var(c) => Ok(Op::Var(c)),
            LParen => {
                let expr = self.or().map(|o| o.enclose(true));
                self.tokens
                    .next_if(|t| matches!(t, RParen))
                    .ok_or(ExpectedCloseParent)
                    .and(expr)
            }
            _ => Err(UnexpectedToken),
        }
    }

    fn or(&mut self) -> Result<Op, ParseError> {
        use Op::{Const, Or, Var};
        let mut lhs = self.and()?;
        while let Some(tok) = self
            .tokens
            .next_if(|t| matches!(t, Token::Plus | Token::BPlus))
        {
            let rhs = self.and()?;
            lhs = match (lhs, rhs) {
                (Var(v1), Var(v2)) if v1 == v2 => Var(v1),
                (Const(true), _) | (_, Const(true)) => Const(true),
                (Const(false), op) | (op, Const(false)) => op,
                (lhs, rhs) => Or {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
            if tok == Token::BPlus {
                negate!(lhs);
            }
        }
        Ok(lhs)
    }

    fn and(&mut self) -> Result<Op, ParseError> {
        use Op::{And, Const, Var};
        let mut lhs = self.xor()?;
        while let Some(tok) = self
            .tokens
            .next_if(|t| matches!(t, Token::Star | Token::BStar))
        {
            let rhs = self.xor()?;
            lhs = match (lhs, rhs) {
                (Var(v1), Var(v2)) if v1 == v2 => Var(v1),
                (Const(true), op) | (op, Const(true)) => op,
                (Const(false), _) | (_, Const(false)) => Const(false),
                (lhs, rhs) => And {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
            if tok == Token::BStar {
                negate!(lhs);
            }
        }
        Ok(lhs)
    }

    fn xor(&mut self) -> Result<Op, ParseError> {
        use Op::{Const, Var, Xor};
        let mut lhs = self.not()?;
        while let Some(tok) = self
            .tokens
            .next_if(|t| matches!(t, Token::Caret | Token::BCaret))
        {
            let rhs = self.not()?;
            lhs = match (lhs, rhs) {
                (Var(v1), Var(v2)) if v1 == v2 => Const(false),
                (lhs, rhs) => Xor {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
            if tok == Token::BCaret {
                negate!(lhs);
            }
        }
        Ok(lhs)
    }

    fn not(&mut self) -> Result<Op, ParseError> {
        if self.tokens.next_if(|t| matches!(t, Token::Bang)).is_some() {
            let expr = self.not()?;
            match expr {
                Op::Const(b) => Ok(Op::Const(!b)),
                Op::Not(op) => Ok(*op),
                Op::Or { .. } | Op::And { .. } | Op::Xor { .. } => {
                    Ok(Op::Not(Box::new(expr.enclose(false))))
                }
                Op::Var(_) => Ok(Op::Not(Box::new(expr))),
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
    BPlus,
    Star,
    BStar,
    Caret,
    BCaret,
    LParen,
    RParen,
    Const(bool),
    Var(char),
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Op {
    Const(bool),
    Var(char),
    Not(Box<Op>),
    Or {
        lhs: Box<Op>,
        rhs: Box<Op>,
    },
    Xor {
        lhs: Box<Op>,
        rhs: Box<Op>,
    },
    And {
        lhs: Box<Op>,
        rhs: Box<Op>,
    },
}

impl Op {
    /// returns a list of variables, sorted in alphabetical order
    fn variables(&self) -> Vec<char> {
        match self {
            Op::Const(_) => Vec::new(),
            Op::Var(name) => vec![*name],
            Op::Not(op) => op.variables(),
            Op::Or { lhs, rhs, .. } | Op::And { lhs, rhs, .. } | Op::Xor { lhs, rhs, .. } => {
                combine_sort_unique(lhs.variables(), rhs.variables())
            }
        }
    }

    fn eval(&self, vars: &HashMap<char, bool>) -> Option<bool> {
        match self {
            Op::Const(b) => Some(*b),
            Op::Var(c) => vars.get(c).copied(),
            Op::Not(op) => op.eval(vars).map(bool::not),
            Op::Or { lhs, rhs, .. } => Some((lhs.eval(vars)?) || (rhs.eval(vars)?)),
            Op::And { lhs, rhs, .. } => Some((lhs.eval(vars)?) && (rhs.eval(vars)?)),
            Op::Xor { lhs, rhs, .. } => Some((lhs.eval(vars)?) ^ (rhs.eval(vars)?)),
        }
    }

    fn enclose(self, _enclose: bool) -> Self {
        let enclose = true;
        match self {
            Op::Or { lhs, rhs, .. } => Op::Or { lhs, rhs },
            Op::Xor { lhs, rhs, .. } => Op::Xor { lhs, rhs },
            Op::And { lhs, rhs, .. } => Op::And { lhs, rhs },
            Op::Not(op) => Op::Not(Box::new(op.enclose(enclose))),
            _ => self,
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Const(b) => f.write_char(bool_digit(*b)),
            Op::Var(name) => f.write_fmt(format_args!("{name}")),
            Op::Not(op) => match *op.to_owned() {
                Op::Or { lhs, rhs } => f.write_str(&format!("({lhs} !+ {rhs})")),
                Op::Xor { lhs, rhs } => f.write_str(&format!("({lhs} !^ {rhs})")),
                Op::And { lhs, rhs } => f.write_str(&format!("({lhs} !* {rhs})")),
                _ => f.write_fmt(format_args!("!{op}")),
            },
            Op::Or { lhs, rhs } => f.write_str(&format!("({lhs} + {rhs})")),
            Op::And { lhs, rhs } => f.write_str(&format!("({lhs} * {rhs})")),
            Op::Xor { lhs, rhs } => f.write_str(&format!("({lhs} ^ {rhs})")),
        }
    }
}
