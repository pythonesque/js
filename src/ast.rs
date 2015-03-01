use std::fmt;
use std::ops::{Deref, DerefMut};

pub struct Annotated<Ann, T>(pub Ann, pub T);

impl<Ann, T> fmt::Debug for Annotated<Ann, T> where T: fmt::Debug {
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

pub fn finish<'a, Ann, T>(start: &'a <Ann as Annotation>::Start,
                          ctx: &'a mut <Ann as Annotation>::Ctx,
                          inner: T,
                         ) -> Annotated<Ann, T>
    where Ann: Annotation,
{
    Annotated(Annotation::finish(start, ctx), inner)
}

impl<Ann, T> Deref for Annotated<Ann, T> {
    type Target = T;

    fn deref<'a>(&'a self) -> &'a T { &self.1 }
}

impl<Annotation, T> DerefMut for Annotated<Annotation, T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.1 }
}

pub type IdentifierNode<'a, Ann> = Annotated<Ann, Identifier<'a>>;
pub type Identifier<'a> = &'a str;

pub type BindingElementNode<'a, Ann> = Annotated<Ann, BindingElement<'a, Ann>>;
#[derive(Debug)]
pub enum BindingElement<'a, Ann> {
    SingleName(IdentifierNode<'a, Ann>, ExpressionNode<'a, Ann>),
    Uninitialized(IdentifierNode<'a, Ann>),
}

pub type VariableDeclarationNode<'a, Ann> = Annotated<Ann, VariableDeclaration<'a, Ann>>;
#[derive(Debug)]
pub struct VariableDeclaration<'a, Ann> {
    pub declarations: Vec<BindingElementNode<'a, Ann>>,
    pub kind: VariableDeclarationKind,
}

#[derive(Debug)]
pub enum VariableDeclarationKind {
    Var,
    //Const,
    //Let,
}

#[derive(Debug)]
pub struct Function<'a, Ann> {
    pub params: Vec<IdentifierNode<'a, Ann>>,
    pub defaults: Vec<BindingElementNode<'a, Ann>>,
    pub rest: Option<IdentifierNode<'a, Ann>>,
    pub body: BlockNode<'a, Ann>,
}

#[derive(Debug)]
pub struct RegExp;

pub type PropertyNode<'a, Ann> = Annotated<Ann, Property<'a, Ann>>;
#[derive(Debug)]
pub enum Property<'a, Ann> {
    Computed(ExpressionNode<'a, Ann>),
    Identifier(&'a str),
    String(&'a str),
    Numeric(f64),
    EscapedString(&'a [u16]),
}

pub type PropertyDefinitionNode<'a, Ann> =
    Annotated<Ann, PropertyDefinition<'a, Ann>>;
#[derive(Debug)]
pub enum PropertyDefinition<'a, Ann> {
    //Identifier(Identifier<'a>),
    Property(PropertyNode<'a, Ann>, ExpressionNode<'a, Ann>),
}

pub type ExpressionNode<'a, Ann> = Annotated<Ann, Expression<'a, Ann>>;
#[derive(Debug)]
pub enum Expression<'a, Ann> {
    This,
    Identifier(&'a str),
    String(&'a str),
    Bool(bool),
    Null,
    EscapedString(&'a [u16]),
    Number(f64),
    Function(Option<IdentifierNode<'a, Ann>>, Function<'a, Ann>),
    Member(Box<(ExpressionNode<'a, Ann>, PropertyNode<'a, Ann>)>),
    Assignment(AssignOp, Box<(ExpressionNode<'a, Ann>, ExpressionNode<'a, Ann>)>),
    Call(Box<ExpressionNode<'a, Ann>>, Vec<ExpressionNode<'a, Ann>>),
    Array(Vec<Option<ExpressionNode<'a, Ann>>>),
    Object(Vec<PropertyDefinitionNode<'a, Ann>>),
    New(Box<ExpressionNode<'a, Ann>>, Vec<ExpressionNode<'a, Ann>>),
    Binary(BinOp, Box<(ExpressionNode<'a, Ann>, ExpressionNode<'a, Ann>)>),
    Conditional(Box<(ExpressionNode<'a, Ann>, ExpressionNode<'a, Ann>, ExpressionNode<'a, Ann>)>),
    Unary(UnOp, Box<ExpressionNode<'a, Ann>>),
    Update(UpdateOp, UpdateType, Box<ExpressionNode<'a, Ann>>),
    Seq(Vec<ExpressionNode<'a, Ann>>),
    RegExp(RegExp),
}

pub type BlockNode<'a, Ann> = Annotated<Ann, Block<'a, Ann>>;
pub type Block<'a, Ann> = Vec<StatementListItemNode<'a, Ann>>;

pub type StatementNode<'a, Ann> = Annotated<Ann, Statement<'a, Ann>>;
#[derive(Debug)]
pub enum Statement<'a, Ann> {
    Empty,
    Block(Block<'a, Ann>),
    Variable(Vec<BindingElementNode<'a, Ann>>),
    Expression(ExpressionNode<'a, Ann>),
    Return(Option<ExpressionNode<'a, Ann>>),
    If(ExpressionNode<'a, Ann>, Box<StatementNode<'a, Ann>>, Option<Box<StatementNode<'a, Ann>>>),
    For(VariableDeclarationNode<'a, Ann>, Option<ExpressionNode<'a, Ann>>,
        Option<ExpressionNode<'a, Ann>>, Box<StatementNode<'a, Ann>>),
    ForExp(Option<ExpressionNode<'a, Ann>>, Option<ExpressionNode<'a, Ann>>,
        Option<ExpressionNode<'a, Ann>>, Box<StatementNode<'a, Ann>>),
    ForIn(VariableDeclarationNode<'a, Ann>, ExpressionNode<'a, Ann>, Box<StatementNode<'a, Ann>>),
    ForExpIn(ExpressionNode<'a, Ann>, ExpressionNode<'a, Ann>, Box<StatementNode<'a, Ann>>), 
}

pub type StatementListItemNode<'a, Ann> = Annotated<Ann, StatementListItem<'a, Ann>>;
#[derive(Debug)]
pub enum StatementListItem<'a, Ann> {
    Statement(StatementNode<'a, Ann>),
    Function(IdentifierNode<'a, Ann>, Function<'a, Ann>),
}

pub type ScriptNode<'a, Ann> = Annotated<Ann, Script<'a, Ann>>;
pub type Script<'a, Ann> = Vec<StatementListItemNode<'a, Ann>>;

#[derive(Copy, Debug)]
pub enum AssignOp {
    Eq,
    TimesEq, DivEq, ModEq, PlusEq, MinusEq,
    LtLtEq, GtGtEq, GtGtGtEq,
    AndEq, XorEq, OrEq,
}

#[derive(Copy, Debug)]
pub enum BinOp {
    OrOr,
    AndAnd,
    Or,
    Xor,
    And,
    EqEq, NEq, EqEqEq, NEqEq,
    Lt, Gt, LtEq, GtEq, InstanceOf,
    In,
    LtLt, GtGt, GtGtGt,
    Plus, Minus,
    Times, Div, Mod,
}

#[derive(Copy, Debug)]
pub enum UnOp {
    Delete,
    Void,
    TypeOf,
    Plus,
    Minus,
    Tilde,
    Not,
}

#[derive(Copy, Debug)]
pub enum UpdateType {
    Prefix,
    Postfix,
}

#[derive(Copy, Debug)]
pub enum UpdateOp {
    PlusPlus,
    MinusMinus,
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
