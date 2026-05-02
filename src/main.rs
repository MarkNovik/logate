use std::collections::HashMap;

fn main() -> anyhow::Result<()> {
    eprintln!("Enter expression");
    let line = read_line()?;
    let tokens = tokenize(&line)?;
    let mut parser = Parser::new(tokens.into_iter());
    let expr = parser.expr()?.simplify();
    eprintln!("FN = {expr}");
    print_truth_table(&expr);
    Ok(())
}

fn read_line() -> anyhow::Result<String> {
    let mut buf = String::new();
    let _ = std::io::stdin().read_line(&mut buf)?;
    Ok(buf.trim_end_matches('\n').to_string())
}

fn print_truth_table(expr: &Op) {
    let vars = {
        let mut vars = expr.variables();
        vars.sort();
        Vars::new(vars)
    };
    let colors = generate_colors(vars.names());
    println!(
        "{}FN",
        vars.names()
            .iter()
            .flat_map(|c| [colorize(*c, colors.get(c).unwrap()), "\t".to_string()])
            .collect::<String>()
    );
    vars.for_each(|state| {
        let mut pairs = state.iter().collect::<Vec<_>>();
        pairs.sort_by_key(|(k, _)| *k);

        print!(
            "{}",
            pairs
                .into_iter()
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
    T: std::fmt::Display,
{
    use inline_colorization::color_reset;
    use std::io::{stdout, IsTerminal};

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
            '|' => Token::Or,
            '&' => Token::And,
            '^' => Token::Xor,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '!' => {
                match chars
                    .next_if(|(_, c)| matches!(c, '^' | '|' | '&'))
                    .map(|(_, c)| c)
                {
                    Some('|') => Token::Nor,
                    Some('&') => Token::Nand,
                    Some('^') => Token::Xnor,
                    _ => Token::Bang,
                }
            }
            v if v.is_alphabetic() => Token::Var(v.to_ascii_uppercase()),
            ws if ws.is_whitespace() => continue,
            _ => return Err(TokenizingError::UnexpectedChar(ch, index)),
        });
    }
    Ok(tokens)
}

fn combine_sort_unique<T: Ord + Clone + std::hash::Hash>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    let mut res = std::collections::HashSet::new();
    res.extend(a);
    res.extend(b);
    let mut res = Vec::from_iter(res);
    res.sort();
    res
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

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
enum TokenizingError {
    UnexpectedChar(char, usize),
}

impl std::fmt::Display for TokenizingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenizingError::UnexpectedChar(char, index) => f.write_fmt(format_args!(
                "Unexpected character `{char}` at index {index}"
            )),
        }
    }
}

impl std::error::Error for TokenizingError {}

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
    tokens: std::iter::Peekable<I>,
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
            Err(ParseError::UnparsedToken)
        } else {
            expr
        }
    }

    fn term(&mut self) -> Result<Op, ParseError> {
        use ParseError::{ExpectedCloseParent, ReachedEOF, UnexpectedToken};
        use Token::{Const, LParen, RParen, Var};

        match self.tokens.next().ok_or(ReachedEOF)? {
            Const(b) => Ok(Op::Const(b)),
            Var(c) => Ok(Op::Var(c)),
            LParen => {
                let expr = self.or().map(|o| o);
                self.tokens
                    .next_if(|t| matches!(t, RParen))
                    .ok_or(ExpectedCloseParent)
                    .and(expr)
            }
            _ => Err(UnexpectedToken),
        }
    }

    fn or(&mut self) -> Result<Op, ParseError> {
        let mut lhs = self.and()?;
        while let Some(tok) = self.tokens.next_if(|t| matches!(t, Token::Or | Token::Nor)) {
            let rhs = self.and()?;

            lhs = match tok {
                Token::Or => Op::or(lhs, rhs),
                Token::Nor => Op::not(Op::or(lhs, rhs)),
                _ => unreachable!("Program enters here only if tok is Or or Nor"),
            }
        }
        Ok(lhs)
    }

    fn and(&mut self) -> Result<Op, ParseError> {
        let mut lhs = self.xor()?;
        while let Some(tok) = self
            .tokens
            .next_if(|t| matches!(t, Token::And | Token::Nand))
        {
            let rhs = self.xor()?;
            lhs = match tok {
                Token::And => Op::and(lhs, rhs),
                Token::Nand => Op::not(Op::and(lhs, rhs)),
                _ => unreachable!("Program enters here only if tok is And or Nand"),
            }
        }
        Ok(lhs)
    }

    fn xor(&mut self) -> Result<Op, ParseError> {
        let mut lhs = self.not()?;
        while let Some(tok) = self
            .tokens
            .next_if(|t| matches!(t, Token::Xor | Token::Xnor))
        {
            let rhs = self.not()?;
            lhs = match tok {
                Token::Xor => Op::xor(lhs, rhs),
                Token::Xnor => Op::not(Op::xor(lhs, rhs)),
                _ => unreachable!("Program enters here only if tok is Xor or Xnor"),
            }
        }
        Ok(lhs)
    }

    fn not(&mut self) -> Result<Op, ParseError> {
        if self.tokens.next_if(|t| matches!(t, Token::Bang)).is_some() {
            self.not().map(Op::not)
        } else {
            self.term()
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Token {
    Bang,
    Or,
    Nor,
    And,
    Nand,
    Xor,
    Xnor,
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
    Or { lhs: Box<Op>, rhs: Box<Op> },
    Xor { lhs: Box<Op>, rhs: Box<Op> },
    And { lhs: Box<Op>, rhs: Box<Op> },
}

impl Op {
    fn not(op: Op) -> Self {
        Op::Not(Box::new(op))
    }

    fn or(lhs: Op, rhs: Op) -> Self {
        Op::Or {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn and(lhs: Op, rhs: Op) -> Self {
        Op::And {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn xor(lhs: Op, rhs: Op) -> Self {
        Op::Xor {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    /// returns a list of variables, sorted in alphabetical order
    fn variables(&self) -> Vec<char> {
        match self {
            Op::Const(_) => Vec::new(),
            Op::Var(name) => vec![*name],
            Op::Not(op) => op.variables(),
            Op::Or { lhs, rhs } | Op::And { lhs, rhs } | Op::Xor { lhs, rhs } => {
                combine_sort_unique(lhs.variables(), rhs.variables())
            }
        }
    }

    fn eval(&self, vars: &HashMap<char, bool>) -> Option<bool> {
        use std::ops::Not;

        match self {
            Op::Const(b) => Some(*b),
            Op::Var(c) => vars.get(c).copied(),
            Op::Not(op) => op.eval(vars).map(bool::not),
            Op::Or { lhs, rhs } => Some((lhs.eval(vars)?) || (rhs.eval(vars)?)),
            Op::And { lhs, rhs } => Some((lhs.eval(vars)?) && (rhs.eval(vars)?)),
            Op::Xor { lhs, rhs } => Some((lhs.eval(vars)?) ^ (rhs.eval(vars)?)),
        }
    }

    fn simplify(self) -> Self {
        match self {
            Op::Const(_) => self,
            Op::Var(_) => self,
            Op::Not(op) => {
                let op = op.simplify();
                match op {
                    Op::Const(b) => Op::Const(!b),
                    Op::Not(op) => *op,
                    Op::Or { lhs, rhs } => {
                        Op::and(Op::not(*lhs).simplify(), Op::not(*rhs).simplify())
                    }
                    Op::And { lhs, rhs } => {
                        Op::or(Op::not(*lhs).simplify(), Op::not(*rhs).simplify())
                    }
                    op @ (Op::Var(_) | Op::Xor { .. }) => Op::Not(Box::new(op)),
                }
            }
            Op::Or { lhs, rhs } => {
                let lhs = lhs.simplify();
                let rhs = rhs.simplify();
                if matches!(lhs, Op::Const(false)) {
                    rhs
                } else if matches!(rhs, Op::Const(false)) {
                    lhs
                } else if matches!(rhs, Op::Const(true)) || matches!(lhs, Op::Const(true)) {
                    Op::Const(true)
                } else if lhs == rhs {
                    lhs
                }
                // A | !A => 1, !A | A => 1
                else if matches!((&lhs, &rhs), (Op::Var(a), Op::Not(op)) | (Op::Not(op), Op::Var(a)) if matches!(op.as_ref(), Op::Var(b) if a == b))
                {
                    Op::Const(true)
                } else {
                    Op::Or {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                }
            }
            Op::Xor { lhs, rhs } => {
                let lhs = lhs.simplify();
                let rhs = rhs.simplify();
                if lhs == rhs {
                    Op::Const(false)
                } else if matches!(lhs, Op::Const(false)) {
                    rhs
                } else if matches!(rhs, Op::Const(false)) {
                    lhs
                } else if matches!(lhs, Op::Const(true)) {
                    Op::not(rhs).simplify()
                } else if matches!(rhs, Op::Const(true)) {
                    Op::not(lhs).simplify()
                }
                // a ^ !a, !a ^a => 1
                else if matches!((&lhs, &rhs), (Op::Var(a), Op::Not(op)) | (Op::Not(op), Op::Var(a)) if matches!(op.as_ref(), Op::Var(b) if a == b))
                {
                    Op::Const(true)
                } else {
                    Op::Xor {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                }
            }
            Op::And { lhs, rhs } => {
                let lhs = lhs.simplify();
                let rhs = rhs.simplify();
                if matches!(lhs, Op::Const(true)) {
                    rhs
                } else if matches!(rhs, Op::Const(true)) {
                    lhs
                } else if matches!(rhs, Op::Const(false)) || matches!(lhs, Op::Const(false)) {
                    Op::Const(false)
                } else if lhs == rhs {
                    lhs
                }
                // A & !A => 0, !A & A => 0
                else if matches!((&lhs, &rhs), (Op::Var(a), Op::Not(op)) | (Op::Not(op), Op::Var(a)) if matches!(op.as_ref(), Op::Var(b) if a == b))
                {
                    Op::Const(false)
                } else {
                    Op::And {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                }
            }
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

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
