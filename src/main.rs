use std::collections::HashMap;

fn exename() -> String {
    // SAFETY: First argument of args is executable name and is always present.
    unsafe { std::env::args().next().unwrap_unchecked() }
}

fn print_usage() {
    println!("Usage: {} <-h|line>", exename());
    println!("Args:");
    println!("\t-h   - print this message.");
    println!("\tline - parse the argument as logic expression and print the truth table.");
}

fn cond(a: bool, b: bool) -> bool {
    (!a) || b
}

fn rev_cond(a: bool, b: bool) -> bool {
    b && (!a)
}

fn bi_cond(a: bool, b: bool) -> bool {
    !(a ^ b)
}

fn main() -> anyhow::Result<()> {
    use std::process::exit;

    let mut args = std::env::args().skip(1);
    let line = Box::leak(Box::new({
        match args.next() {
            Some(h) if h.as_str() == "-h" => {
                print_usage();
                exit(0)
            }
            Some(l) => l,
            None => {
                print_usage();
                eprintln!("ERR: No arguments provided");
                exit(1)
            }
        }
    }));

    let tokens = tokenize(line)?;
    let mut parser = Parser::new(line, tokens.into_iter());
    let expr = parser.expr()?.simplify();
    eprintln!("FN = {expr}");
    if !matches!(expr, Op::Const(_)) {
        print_truth_table(&expr, &expr.to_string());
    }
    Ok(())
}

fn print_truth_table(expr: &Op, fn_name: &str) {
    const CONNECTION_CHAR: char = '\t';

    let vars = {
        let mut vars = expr.variables();
        vars.sort();
        Vars::new(vars)
    };
    let colors = generate_colors(vars.names());
    println!(
        "{}{fn_name}",
        vars.names()
            .iter()
            .flat_map(|c| [
                colorize(*c, colors.get(c).unwrap()),
                CONNECTION_CHAR.to_string()
            ])
            .collect::<String>()
    );
    vars.for_each(|state| {
        let mut pairs = state.iter().collect::<Vec<_>>();
        pairs.sort_by_key(|(k, _)| *k);

        print!(
            "{}",
            pairs
                .into_iter()
                .flat_map(|(_, v)| [bool_digit(*v), CONNECTION_CHAR])
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

fn tokenize(input: &str) -> Result<Vec<Located<Token>>, TokenizingError<'_>> {
    let mut chars = input.chars().enumerate().peekable();
    let mut tokens = Vec::new();
    while let Some((index, ch)) = chars.next() {
        tokens.push((
            index,
            match ch {
                '1' | '0' => Token::Const(ch == '1'),
                '|' => Token::Or,
                '&' => Token::And,
                '^' => Token::Xor,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '~' => Token::Not,
                '!' => {
                    match chars
                        .next_if(|(_, c)| matches!(c, '^' | '|' | '&'))
                        .map(|(_, c)| c)
                    {
                        Some('|') => Token::Nor,
                        Some('&') => Token::Nand,
                        Some('^') => Token::Xnor,
                        _ => Token::Not,
                    }
                }
                '<' => {
                    if chars.next_if(|(_, c)| *c == '-').is_some() {
                        if chars.next_if(|(_, c)| *c == '>').is_some() {
                            Token::BiCond
                        } else {
                            Token::RevCond
                        }
                    } else {
                        return Err(TokenizingError::unfinished_token(input, index + 1, '-'));
                    }
                }
                '-' => {
                    if chars.next_if(|(_, c)| *c == '>').is_some() {
                        Token::Cond
                    } else {
                        return Err(TokenizingError::unfinished_token(input, index + 1, '>'));
                    }
                }
                v if v.is_alphabetic() => Token::Var(v.to_ascii_uppercase()),
                ws if ws.is_whitespace() => continue,
                _ => {
                    return Err(TokenizingError::unexpected_char(input, index, ch));
                }
            },
        ));
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
        'T' //'1'
    } else {
        'F' // '0'
    }
}

#[derive(Debug)]
struct ParseError {
    src: Box<str>,
    position: usize,
    message: String,
    // ReachedEOF,
    // ExpectedCloseParent,
    // UnexpectedToken,
    // UnparsedToken,
}

impl ParseError {
    fn _new(src: &str, position: usize, message: String) -> ParseError {
        Self {
            src: src.into(),
            position,
            message,
        }
    }

    fn unexpected_eof(src: &str, position: usize) -> Self {
        Self {
            src: src.into(),
            position,
            message: "Unexpected EOF".to_string(),
        }
    }

    fn expected_rparen(src: &str, position: usize) -> Self {
        Self {
            src: src.into(),
            position,
            message: "Expected `)`".to_string(),
        }
    }

    fn unexpected_token(src: &str, position: usize, expected: &str, found: Token) -> Self {
        Self {
            src: src.into(),
            position,
            message: format!(
                "Unexpected token `{found}` at index {position}, expected `{expected}`"
            ),
        }
    }

    fn unparsed_token(src: &str, position: usize, token: Token) -> Self {
        Self {
            src: src.into(),
            position,
            message: format!(
                "Unexpected token `{token}` after finished expression, at index {position}"
            ),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ParseError {
            src,
            position,
            message,
        } = self;
        f.write_fmt(format_args!(
            "{message}\n{src}\n{spaces}^",
            spaces = " ".repeat(*position)
        ))
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
struct TokenizingError<'src> {
    src: &'src str,
    position: usize,
    message: String,
}

impl<'src> TokenizingError<'src> {
    fn _new(src: &'src str, position: usize, message: String) -> Self {
        Self {
            src,
            position,
            message,
        }
    }

    fn unexpected_char(src: &'src str, position: usize, unexpected: char) -> Self {
        Self {
            src,
            position,
            message: format!("Unexpected character `{unexpected}` at index {position}"),
        }
    }

    fn unfinished_token(src: &'src str, position: usize, expected: char) -> Self {
        Self {
            src,
            position,
            message: format!("Unfinished token at index {position}, expected `{expected}`"),
        }
    }
}

impl std::fmt::Display for TokenizingError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TokenizingError {
            src,
            position,
            message,
        } = self;
        f.write_fmt(format_args!(
            "{message}\n{src}\n{spaces}^",
            spaces = " ".repeat(*position)
        ))
    }
}

impl std::error::Error for TokenizingError<'_> {}

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

struct Parser<'src, I: Iterator<Item = Located<Token>>> {
    src: &'src str,
    tokens: std::iter::Peekable<I>,
}

impl<'src, I: Iterator<Item = Located<Token>>> Parser<'src, I> {
    fn new(src: &'src str, tokens: I) -> Self {
        Self {
            src,
            tokens: tokens.peekable(),
        }
    }

    pub fn expr(&mut self) -> Result<Op, ParseError> {
        let src = self.src;
        let expr = self.or()?;
        match self.tokens.next() {
            Some((p, t)) => Err(ParseError::unparsed_token(src, p, t)),
            None => Ok(expr),
        }
    }

    fn term(&mut self) -> Result<Op, ParseError> {
        use Token::{Const, LParen, RParen, Var};

        match self
            .tokens
            .next()
            .ok_or_else(|| ParseError::unexpected_eof(self.src, self.src.len()))?
        {
            (_p, Const(b)) => Ok(Op::Const(b)),
            (_p, Var(c)) => Ok(Op::Var(c)),
            (_p, LParen) => {
                let expr = self.or().map(|o| o);
                self.tokens
                    .next_if(|(_p, t)| matches!(t, RParen))
                    .ok_or_else(|| ParseError::expected_rparen(self.src, self.src.len()))
                    .and(expr)
            }
            (p, t) => Err(ParseError::unexpected_token(
                self.src,
                p,
                "a constant, a variable or an expression",
                t,
            )),
        }
    }

    fn or(&mut self) -> Result<Op, ParseError> {
        let mut lhs = self.and()?;
        while let Some((_p, tok)) = self
            .tokens
            .next_if(|(_p, t)| matches!(t, Token::Or | Token::Nor))
        {
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
        while let Some((_p, tok)) = self
            .tokens
            .next_if(|(_p, t)| matches!(t, Token::And | Token::Nand))
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
        let mut lhs = self.cond()?;
        while let Some((_p, tok)) = self
            .tokens
            .next_if(|(_p, t)| matches!(t, Token::Xor | Token::Xnor))
        {
            let rhs = self.cond()?;
            lhs = match tok {
                Token::Xor => Op::xor(lhs, rhs),
                Token::Xnor => Op::not(Op::xor(lhs, rhs)),
                _ => unreachable!("Program enters here only if tok is Xor or Xnor"),
            }
        }
        Ok(lhs)
    }

    fn cond(&mut self) -> Result<Op, ParseError> {
        let mut lhs = self.not()?;
        while let Some((_p, tok)) = self
            .tokens
            .next_if(|(_p, t)| matches!(t, Token::Cond | Token::BiCond | Token::RevCond))
        {
            let rhs = self.not()?;
            lhs = match tok {
                Token::BiCond => Op::bi_cond(lhs, rhs),
                Token::RevCond => Op::rev_cond(lhs, rhs),
                Token::Cond => Op::cond(lhs, rhs),
                _ => unreachable!("Program enters here if tok is Cond, RevCond or BiCond"),
            }
        }
        Ok(lhs)
    }

    fn not(&mut self) -> Result<Op, ParseError> {
        if self
            .tokens
            .next_if(|(_p, t)| matches!(t, Token::Not))
            .is_some()
        {
            self.not().map(Op::not)
        } else {
            self.term()
        }
    }
}

type Located<T> = (usize, T);

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Token {
    Not,
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
    BiCond,
    RevCond,
    Cond,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Token::Not => "~",
            Token::Or => "|",
            Token::Nor => "!|",
            Token::And => "&",
            Token::Nand => "!&",
            Token::Xor => "^",
            Token::Xnor => "!^",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Const(v) => &format!("{}", bool_digit(*v)),
            Token::Var(name) => &name.to_string(),
            Token::BiCond => "<->",
            Token::RevCond => "<-",
            Token::Cond => "->",
        };
        f.write_str(str)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Op {
    Const(bool),
    Var(char),
    Not(Box<Op>),
    Or { lhs: Box<Op>, rhs: Box<Op> },
    Xor { lhs: Box<Op>, rhs: Box<Op> },
    And { lhs: Box<Op>, rhs: Box<Op> },
    Cond { lhs: Box<Op>, rhs: Box<Op> },
    RevCond { lhs: Box<Op>, rhs: Box<Op> },
    BiCond { lhs: Box<Op>, rhs: Box<Op> },
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

    fn cond(lhs: Op, rhs: Op) -> Self {
        Op::Cond {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn rev_cond(lhs: Op, rhs: Op) -> Self {
        Op::RevCond {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn bi_cond(lhs: Op, rhs: Op) -> Self {
        Op::BiCond {
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
            Op::Or { lhs, rhs }
            | Op::And { lhs, rhs }
            | Op::Xor { lhs, rhs }
            | Op::Cond { lhs, rhs }
            | Op::RevCond { lhs, rhs }
            | Op::BiCond { lhs, rhs } => combine_sort_unique(lhs.variables(), rhs.variables()),
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
            Op::Cond { lhs, rhs } => Some(cond(lhs.eval(vars)?, rhs.eval(vars)?)),
            Op::RevCond { lhs, rhs } => Some(rev_cond(lhs.eval(vars)?, rhs.eval(vars)?)),
            Op::BiCond { lhs, rhs } => Some(bi_cond(lhs.eval(vars)?, rhs.eval(vars)?)),
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
                    _ => Op::not(op),
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

            //TODO: do some additional simplification checks
            Op::Cond { lhs, rhs } => Op::cond(lhs.simplify(), rhs.simplify()),
            Op::RevCond { lhs, rhs } => Op::rev_cond(lhs.simplify(), rhs.simplify()),
            Op::BiCond { lhs, rhs } => Op::bi_cond(lhs.simplify(), rhs.simplify()),
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        match self {
            Op::Const(b) => f.write_char(bool_digit(*b)),
            Op::Var(name) => f.write_fmt(format_args!("{name}")),
            Op::Not(ref op) => match op.as_ref() {
                Op::Var(v) => f.write_fmt(format_args!("~{v}")),
                op => f.write_fmt(format_args!("~({op})")),
            },
            Op::Or { lhs, rhs } => f.write_str(&format!("({lhs} | {rhs})")),
            Op::And { lhs, rhs } => f.write_str(&format!("({lhs} & {rhs})")),
            Op::Xor { lhs, rhs } => f.write_str(&format!("({lhs} ^ {rhs})")),
            Op::Cond { lhs, rhs } => f.write_str(&format!("({lhs} -> {rhs})")),
            Op::RevCond { lhs, rhs } => f.write_str(&format!("({lhs} <- {rhs})")),
            Op::BiCond { lhs, rhs } => f.write_str(&format!("({lhs} <-> {rhs})")),
        }
    }
}
