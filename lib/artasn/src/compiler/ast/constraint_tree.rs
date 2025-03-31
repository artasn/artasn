#![allow(unused)]

use std::{collections::BTreeMap, iter::Peekable, marker::PhantomData};

use crate::compiler::parser::*;

lazy_static::lazy_static! {
    pub static ref PRATT: PrattParser = PrattParser::new()
        .op(Op::infix(SetOperator::Union))
        .op(Op::infix(SetOperator::Intersection));
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SetOperator {
    Union,
    Intersection,
}

pub enum ElementSetToken<'a> {
    Operand(&'a AstElement<AstSubtypeElement>),
    Operator(AstElement<SetOperator>),
}

type Prec = u32;
const PREC_STEP: Prec = 10;

pub struct Op {
    token: SetOperator,
    affix: Affix,
}

impl Op {
    pub fn prefix(token: SetOperator) -> Self {
        Self {
            token,
            affix: Affix::Prefix,
        }
    }

    pub fn postfix(token: SetOperator) -> Self {
        Self {
            token,
            affix: Affix::Postfix,
        }
    }

    pub fn infix(token: SetOperator) -> Self {
        Self {
            token,
            affix: Affix::Infix,
        }
    }
}

pub enum Affix {
    Prefix,
    Infix,
    Postfix,
}

pub struct PrattParser {
    prec: Prec,
    ops: BTreeMap<SetOperator, (Affix, Prec)>,
    has_prefix: bool,
    has_postfix: bool,
    has_infix: bool,
}

impl PrattParser {
    /// Instantiate a new `PrattParser`.
    pub fn new() -> Self {
        Self {
            prec: PREC_STEP,
            ops: BTreeMap::new(),
            has_prefix: false,
            has_postfix: false,
            has_infix: false,
        }
    }

    /// Add `op` to `PrattParser`.
    pub fn op(mut self, op: Op) -> Self {
        self.prec += PREC_STEP;
        match &op.affix {
            Affix::Prefix => self.has_prefix = true,
            Affix::Postfix => self.has_postfix = true,
            Affix::Infix => self.has_infix = true,
        }
        self.ops.insert(op.token, (op.affix, self.prec));
        self
    }

    /// Maps primary expressions with a closure `primary`.
    pub fn map_primary<'pratt, 'a, 'i, X, T>(
        &'pratt self,
        primary: X,
    ) -> PrattParserMap<'pratt, 'a, 'i, X, T>
    where
        X: FnMut(&'i AstElement<AstSubtypeElement>) -> T,
    {
        PrattParserMap {
            pratt: self,
            primary,
            prefix: None,
            postfix: None,
            infix: None,
            phantom: PhantomData,
        }
    }
}

type PrefixFn<'a, T> = Box<dyn FnMut(AstElement<SetOperator>, T) -> T + 'a>;
type PostfixFn<'a, T> = Box<dyn FnMut(T, AstElement<SetOperator>) -> T + 'a>;
type InfixFn<'a, T> = Box<dyn FnMut(T, AstElement<SetOperator>, T) -> T + 'a>;

/// Product of calling [`map_primary`] on [`PrattParser`], defines how expressions should
/// be mapped.
///
/// [`map_primary`]: struct.PrattParser.html#method.map_primary
/// [`PrattParser`]: struct.PrattParser.html
pub struct PrattParserMap<'pratt, 'a, 'i, F, T>
where
    F: FnMut(&'i AstElement<AstSubtypeElement>) -> T,
{
    pratt: &'pratt PrattParser,
    primary: F,
    prefix: Option<PrefixFn<'a, T>>,
    postfix: Option<PostfixFn<'a, T>>,
    infix: Option<InfixFn<'a, T>>,
    phantom: PhantomData<&'i ()>,
}

impl<'a, 'i, F, T> PrattParserMap<'_, 'a, 'i, F, T>
where
    F: FnMut(&'i AstElement<AstSubtypeElement>) -> T,
{
    /// Maps prefix operators with closure `prefix`.
    pub fn map_prefix<X>(mut self, prefix: X) -> Self
    where
        X: FnMut(AstElement<SetOperator>, T) -> T + 'a,
    {
        self.prefix = Some(Box::new(prefix));
        self
    }

    /// Maps postfix operators with closure `postfix`.
    pub fn map_postfix<X>(mut self, postfix: X) -> Self
    where
        X: FnMut(T, AstElement<SetOperator>) -> T + 'a,
    {
        self.postfix = Some(Box::new(postfix));
        self
    }

    /// Maps infix operators with a closure `infix`.
    pub fn map_infix<X>(mut self, infix: X) -> Self
    where
        X: FnMut(T, AstElement<SetOperator>, T) -> T + 'a,
    {
        self.infix = Some(Box::new(infix));
        self
    }

    /// The last method to call on the provided pairs to execute the Pratt
    /// parser (previously defined using [`map_primary`], [`map_prefix`], [`map_postfix`],
    /// and [`map_infix`] methods).
    ///
    /// [`map_primary`]: struct.PrattParser.html#method.map_primary
    /// [`map_prefix`]: struct.PrattParserMap.html#method.map_prefix
    /// [`map_postfix`]: struct.PrattParserMap.html#method.map_postfix
    /// [`map_infix`]: struct.PrattParserMap.html#method.map_infix
    pub fn parse<P: Iterator<Item = ElementSetToken<'i>>>(&mut self, pairs: P) -> T {
        self.expr(&mut pairs.peekable(), 0)
    }

    fn expr<P: Iterator<Item = ElementSetToken<'i>>>(
        &mut self,
        pairs: &mut Peekable<P>,
        rbp: Prec,
    ) -> T {
        let mut lhs = self.nud(pairs);
        while rbp < self.lbp(pairs) {
            lhs = self.led(pairs, lhs);
        }
        lhs
    }

    /// Null-Denotation
    ///
    /// "the action that should happen when the symbol is encountered
    ///  as start of an expression (most notably, prefix operators)
    fn nud<P: Iterator<Item = ElementSetToken<'i>>>(&mut self, pairs: &mut Peekable<P>) -> T {
        let pair = pairs.next().expect("Pratt parsing expects non-empty Pairs");
        match pair {
            ElementSetToken::Operator(op) => match self.pratt.ops.get(&op.element) {
                Some((Affix::Prefix, prec)) => {
                    let rhs = self.expr(pairs, *prec - 1);
                    match self.prefix.as_mut() {
                        Some(prefix) => prefix(op, rhs),
                        None => panic!("Could not map {:?}, no `.map_prefix(...)` specified", op),
                    }
                }
                _ => panic!("expecting prefix operator, found {:?}", op.element),
            },
            ElementSetToken::Operand(operand) => (self.primary)(operand),
        }
    }

    /// Left-Denotation
    ///
    /// "the action that should happen when the symbol is encountered
    /// after the start of an expression (most notably, infix and postfix operators)"
    fn led<P: Iterator<Item = ElementSetToken<'i>>>(
        &mut self,
        pairs: &mut Peekable<P>,
        lhs: T,
    ) -> T {
        let pair = pairs.next().unwrap();

        match pair {
            ElementSetToken::Operator(op) => match self.pratt.ops.get(&op.element) {
                Some((Affix::Infix, prec)) => {
                    let rhs = self.expr(pairs, *prec);
                    match self.infix.as_mut() {
                        Some(infix) => infix(lhs, op, rhs),
                        None => panic!("Could not map {:?}, no `.map_infix(...)` specified", op),
                    }
                }
                Some((Affix::Postfix, _)) => match self.postfix.as_mut() {
                    Some(postfix) => postfix(lhs, op),
                    None => panic!("Could not map {:?}, no `.map_postfix(...)` specified", op),
                },
                _ => panic!("Expected postfix or infix expression, found {:?}", op),
            },
            ElementSetToken::Operand(_) => {
                panic!("Expected postfix or infix expression, found operand")
            }
        }
    }

    /// Left-Binding-Power
    ///
    /// "describes the symbol's precedence in infix form (most notably, operator precedence)"
    fn lbp<P: Iterator<Item = ElementSetToken<'i>>>(&mut self, pairs: &mut Peekable<P>) -> Prec {
        match pairs.peek() {
            Some(pair) => match pair {
                ElementSetToken::Operator(op) => match self.pratt.ops.get(&op.element) {
                    Some((_, prec)) => *prec,
                    None => panic!("Expected operator, found {:?}", op),
                },
                ElementSetToken::Operand(_) => panic!("Expected operator, found operand"),
            },
            None => 0,
        }
    }
}
