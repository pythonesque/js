use std::fmt;
use std::ops::{Deref, DerefMut};

pub struct Annotated<Annotation, T>(pub Annotation, pub T);

impl<Annotation, T> fmt::Debug for Annotated<Annotation, T> where T: fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.1.fmt(f)
    }
}

pub trait Annotation {
    type Ctx;
    type Start;

    fn start(ctx: &Self::Ctx) -> Self::Start;
    fn finish(start: Self::Start, ctx: &Self::Ctx) -> Self;
}

pub fn finish<Ann, T>(start: <Ann as Annotation>::Start,
                      ctx: &mut <Ann as Annotation>::Ctx,
                      inner: T,
                     ) -> Annotated<Ann, T>
    where Ann: Annotation,
{
    Annotated(Annotation::finish(start, ctx), inner)
}

impl<Annotation, T> Deref for Annotated<Annotation, T> {
    type Target = T;

    fn deref(&self) -> &T { &self.1 }
}

impl<Annotation, T> DerefMut for Annotated<Annotation, T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.1 }
}

pub type IdentifierNode<'a, Annotation> = Annotated<Annotation, Identifier<'a>>;
pub type Identifier<'a> = &'a str;

pub type FunctionNode<'a, Annotation> = Annotated<Annotation, Function<'a, Annotation>>;
#[derive(Debug)]
pub struct Function<'a, Annotation> {
    pub params: Vec<IdentifierNode<'a, Annotation>>,
    //pub defaults: Vec<(&'a str, ExpressionNode<'a>)>,
    pub rest: Option<IdentifierNode<'a, Annotation>>,
    pub body: BlockNode<'a, Annotation>,
}

pub type ExpressionNode<'a, Annotation> = Annotated<Annotation, Expression<'a, Annotation>>;
#[derive(Debug)]
pub enum Expression<'a, Annotation> {
    Identifier(&'a str),
    String(&'a str),
    Number(f64),
    Function(Option<IdentifierNode<'a, Annotation>>, FunctionNode<'a, Annotation>),
}

pub type BlockNode<'a, Annotation> = Annotated<Annotation, Block<'a, Annotation>>;
pub type Block<'a, Annotation> = Vec<StatementListItemNode<'a, Annotation>>;

pub type StatementNode<'a, Annotation> = Annotated<Annotation, Statement<'a, Annotation>>;
#[derive(Debug)]
pub enum Statement<'a, Annotation> {
    Empty,
    Expression(ExpressionNode<'a, Annotation>)
}

pub type StatementListItemNode<'a, Annotation> = Annotated<Annotation, StatementListItem<'a, Annotation>>;
#[derive(Debug)]
pub enum StatementListItem<'a, Annotation> {
    Statement(StatementNode<'a, Annotation>),
    //FunctionDeclaration(IdentifierNode<'a, Annotation>, FunctionNode<'a, Annotation>),
}

pub type ScriptNode<'a, Annotation> = Annotated<Annotation, Script<'a, Annotation>>;
pub type Script<'a, Annotation> = Vec<StatementListItemNode<'a, Annotation>>;

#[derive(Copy, Debug)]
pub enum Tok<'a> {
    EOF,
    Identifier(&'a str),
    NullLiteral,
    BooleanLiteral(bool),
    OctalLiteral(&'a str),
    StringLiteral(&'a str),
    NumericLiteral(f64),

    // Keywords
    If, In, Do,
    Var, For, New, Try, Let,
    This, Else, Case, Void, With, Enum,
    While, Break, Catch, Throw, Const, Yield, Class, Super,
    Return, TypeOf, Delete, Switch, Export, Import,
    Default, Finally, Extends, Function, Continue, Debugger,
    InstanceOf,

    // Strict mode reserved words
    Implements, Interface, Package, Private, Protected, Public, Static,

    Dot,
    Ellipsis,
    LParen,
    LBrack,
    RParen,
    Semi,
    Comma,
    RBrack,
    LBrace,
    RBrace,
    Colon,
    QMark,
    Tilde,

    GtGtGtEq,

    EqEqEq,
    NEqEq,
    GtGtGt,
    LtLtEq,
    GtGtEq,

    AndAnd,
    OrOr,
    EqEq,
    NEq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    PlusPlus,
    MinusMinus,
    LtLt,
    GtGt,
    AndEq,
    OrEq,
    XorEq,
    ModEq,
    LtEq,
    GtEq,
    Arrow,

    Lt,
    Gt,
    Eq,
    Not,
    Plus,
    Minus,
    Times,
    Mod,
    And,
    Or,
    Xor,
    Div,
}
