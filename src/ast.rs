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
    fn finish(start: &Self::Start, ctx: &Self::Ctx) -> Self;
}

pub fn finish<Ann, T>(start: &<Ann as Annotation>::Start,
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

pub type BindingElementNode<'a, Annotation> = Annotated<Annotation, BindingElement<'a, Annotation>>;
#[derive(Debug)]
pub enum BindingElement<'a, Annotation> {
    SingleName(IdentifierNode<'a, Annotation>, ExpressionNode<'a, Annotation>),
    Uninitialized(IdentifierNode<'a, Annotation>),
}

#[derive(Debug)]
pub struct Function<'a, Annotation> {
    pub params: Vec<IdentifierNode<'a, Annotation>>,
    pub defaults: Vec<BindingElementNode<'a, Annotation>>,
    pub rest: Option<IdentifierNode<'a, Annotation>>,
    pub body: BlockNode<'a, Annotation>,
}

pub type PropertyNode<'a, Annotation> = Annotated<Annotation, Property<'a, Annotation>>;
#[derive(Debug)]
pub enum Property<'a, Annotation> {
    Computed(ExpressionNode<'a, Annotation>),
    Identifier(&'a str),
    String(&'a str),
    Numeric(f64),
    EscapedString(&'a [u16]),
}

pub type PropertyDefinitionNode<'a, Annotation> =
    Annotated<Annotation, PropertyDefinition<'a, Annotation>>;
#[derive(Debug)]
pub enum PropertyDefinition<'a, Annotation> {
    //Identifier(Identifier<'a>),
    Property(PropertyNode<'a, Annotation>, ExpressionNode<'a, Annotation>),
}

pub type ExpressionNode<'a, Annotation> = Annotated<Annotation, Expression<'a, Annotation>>;
#[derive(Debug)]
pub enum Expression<'a, Annotation> {
    This,
    Identifier(&'a str),
    String(&'a str),
    Bool(bool),
    Null,
    EscapedString(&'a [u16]),
    Number(f64),
    Function(Option<IdentifierNode<'a, Annotation>>, Function<'a, Annotation>),
    Member(Box<ExpressionNode<'a, Annotation>>, Box<PropertyNode<'a, Annotation>>),
    Assignment(Box<ExpressionNode<'a, Annotation>>, AssignOp, Box<ExpressionNode<'a, Annotation>>),
    Call(Box<ExpressionNode<'a, Annotation>>, Vec<ExpressionNode<'a, Annotation>>),
    Array(Vec<Option<ExpressionNode<'a, Annotation>>>),
    Object(Vec<PropertyDefinitionNode<'a, Annotation>>),
}

pub type BlockNode<'a, Annotation> = Annotated<Annotation, Block<'a, Annotation>>;
pub type Block<'a, Annotation> = Vec<StatementListItemNode<'a, Annotation>>;

pub type StatementNode<'a, Annotation> = Annotated<Annotation, Statement<'a, Annotation>>;
#[derive(Debug)]
pub enum Statement<'a, Annotation> {
    Empty,
    Block(Block<'a, Annotation>),
    Variable(Vec<BindingElementNode<'a, Annotation>>),
    Expression(ExpressionNode<'a, Annotation>),
    Return(Option<ExpressionNode<'a, Annotation>>),
}

pub type StatementListItemNode<'a, Annotation> = Annotated<Annotation, StatementListItem<'a, Annotation>>;
#[derive(Debug)]
pub enum StatementListItem<'a, Annotation> {
    Statement(StatementNode<'a, Annotation>),
    Function(IdentifierNode<'a, Annotation>, Function<'a, Annotation>),
}

pub type ScriptNode<'a, Annotation> = Annotated<Annotation, Script<'a, Annotation>>;
pub type Script<'a, Annotation> = Vec<StatementListItemNode<'a, Annotation>>;

#[derive(Copy, Debug)]
pub enum AssignOp {
    Eq,
    TimesEq, DivEq, ModEq, PlusEq, MinusEq,
    LtLtEq, GtGtEq, GtGtGtEq,
    AndEq, XorEq, OrEq,
}

#[derive(Copy, Debug)]
pub enum Tok<'a> {
    EOF,
    Identifier(&'a str),
    NullLiteral,
    BooleanLiteral(bool),
    StringLiteral(&'a str),
    EscapedStringLiteral(&'a [u16]),
    OctalStringLiteral(&'a [u16]),
    NumericLiteral(f64),
    OctalIntegerLiteral(f64),

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
