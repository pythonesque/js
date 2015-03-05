#![feature(core,collections,rustc_private,unicode)]

extern crate arena;
extern crate unicode;

use arena::TypedArena;

use ast::{
    Annotation,
    AssignOp,
    BindingElement,
    BinOp,
    Block,
    CatchClause,
    finish,
    Identifier,
    Property,
    PropertyDefinition,
    RegExp,
    Statement,
    SwitchCase,
    SwitchCaseTest,
    UnOp,
    UpdateOp,
    UpdateType,
    VariableDeclaration,
    VariableDeclarationKind,
};
use ast::Annotated as A;
use ast::BlockNode as BN;
use ast::BindingElementNode as BEN;
use ast::CatchClauseNode as CCN;
use ast::Expression as E;
use ast::ExpressionNode as EN;
use ast::Function as F;
use ast::IdentifierNode as IN;
use ast::PropertyDefinitionNode as PDN;
use ast::PropertyNode as PN;
use ast::ScriptNode as ScriptN;
use ast::StatementListItem as SLI;
use ast::StatementListItemNode as SLIN;
use ast::StatementNode as SN;
use ast::SwitchCaseNode as SCN;
use ast::Tok as T;
use ast::VariableDeclarationNode as VDN;

use std::char;
use std::error::FromError;
use std::fmt;
use std::mem;
use std::num::{self, ParseFloatError};

use unicode::str::Utf16Encoder;

pub mod ast;

pub type Pos = u32;

macro_rules! tk {
    ($pat:pat) => (Some(Token { ty: $pat , .. }))
}

macro_rules! expect {
    ($e:expr, $($pat:pat),+) => {
        match $e.lookahead {
            $( tk!($pat) => try!($e.lex()), )+
            o => { try!($e.lex()); return Err($e.unexpected_token(o)) }
        }
    }
}

macro_rules! tmatch {
    ($e:expr, $($pat:pat),+) => {
        match $e {
            $( tk!($pat) => true, )+
            _ => false
        }
    }
}

#[derive(Clone, Default)]
struct State<'a> {
    allow_in: bool,
    label_set: Vec<&'a str>,
    parenthesis_count: Pos,
    in_function_body: bool,
    in_iteration: bool,
    in_switch: bool,
    last_comment_start: Option<Pos>
}

pub struct RootCtx {
    string_arena: TypedArena<Vec<u16>>,
    ident_arena: TypedArena<String>,
}

pub struct Ctx<'a, Ann, Start>
{
    source: &'a str,
    index: Pos,
    line_number: Pos,
    line_start: Pos,

    pub start_index: Pos,
    pub start_line_number: Pos,
    pub start_line_start: Pos,
    //length: usize,
    lookahead: Option<Token<'a>>,
    state: State<'a>,

    scanning: bool,
    has_line_terminator: bool,
    pub last_index: Option<Pos>,
    pub last_line_number: Option<Pos>,
    pub last_line_start: Option<Pos>,
    strict: bool,

    rest: &'a str,
    byte_index: usize,
    start_byte_index: usize,
    root: &'a RootCtx,
    binop_stack: Vec<(BinOp, Prec, EN<'a, Ann>, Start)>,
}

#[derive(Copy)]
pub struct Token<'a> {
    pub ty: T<'a>,
    pub line_number: Pos,
    pub line_start: Pos,
    pub start: Pos,
    pub end: Pos
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}: {:?}", self.line_number, self.start - self.line_start + 1, self.ty)
    }
}

/*pub enum Loc {
    No,
    Anon,
    File(&Path)
}*/


pub struct Options;/* {
    range: bool,
    loc: Loc,
    attach_comment: bool,
}*/

#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedToken(Token<'a>),
    UnexpectedChar(char),
    ExpectedHex,
    InvalidUnicode(u32),
    UnexpectedEscape(u32),
    UnterminatedStringLiteral,
    UnexpectedEOF,
    ParseFloat(ParseFloatError),
    StrictFunctionName(Token<'a>),
    StrictReservedWord(Token<'a>),
    StrictOctalLiteral(Token<'a>),
    ObjectPatternAsRestParameter,
    DefaultRestParameter,
    ParameterAfterRestParameter,
    StrictParamName(Token<'a>),
    StrictParamDupe(Token<'a>),
    StrictVarName,
    StrictLHSAssignment(Token<'a>),
    InvalidLHSInArgument,
    IllegalReturn,
    StrictLHSPrefix,
    InvalidLHSInAssignment,
    StrictDelete,
    StrictLHSPostfix,
    UnterminatedRegExp,
    InvalidLHSInForIn,
    NewlineAfterThrow,
    MultipleDefaultsInSwitch,
    StrictCatchVariable,
    NoCatchOrFinally,
    IllegalBreak,
    UnknownLabel(Token<'a>),
    IllegalContinue,
    LabelRedeclaration(Token<'a>),
}

impl<'a> FromError<ParseFloatError> for Error<'a> {
    fn from_error(err: ParseFloatError) -> Self {
        Error::ParseFloat(err)
    }
}

pub type PRes<'a, T> = Result<T, Error<'a>>;

impl RootCtx {
    pub fn new() -> Self {
        RootCtx {
            string_arena: TypedArena::new(),
            ident_arena: TypedArena::new(),
        }
    }
}

fn is_white_space(ch: char) -> bool {
    match ch {
        '\x20' | '\x09' | '\x0B' | '\x0C' | '\u{A0}' |
        '\u{1680}' | '\u{180E}' | '\u{2000}'...'\u{200A}' | '\u{202F}' | '\u{205F}' | '\u{3000}' | '\u{FEFF}' => true,
        _ => false
    }
}

fn is_line_terminator(ch: char) -> bool {
    match ch {
        '\x0A' | '\x0D' | '\u{2028}' | '\u{2029}' => true,
        _ => false
    }
}

const UTF16_DOLLAR: u32 = '$' as u32;
const UTF16__: u32 = '_' as u32;
const UTF16_A: u32 = 'A' as u32;
const UTF16_Z: u32 = 'Z' as u32;
#[allow(non_upper_case_globals)] const UTF16_a: u32 = 'a' as u32;
#[allow(non_upper_case_globals)] const UTF16_z: u32 = 'z' as u32;
const UTF16_BACKSLASH: u32 = '\\' as u32;
const UTF16_0: u32 = '0' as u32;
const UTF16_9: u32 = '9' as u32;

fn is_identifier_start(ch: u32) -> bool {
    match ch {
        UTF16_DOLLAR | UTF16__ |
        UTF16_A ... UTF16_Z |
        UTF16_a ... UTF16_z |
        UTF16_BACKSLASH
        // | ch if non_ascii_regex_that_is_ridiculous
        => true,
        _ => false
    }
}

fn is_identifier_part(ch: u32) -> bool {
    match ch {
        UTF16_DOLLAR | UTF16__ |
        UTF16_A ... UTF16_Z |
        UTF16_a ... UTF16_z |
        UTF16_0 ... UTF16_9 |
        UTF16_BACKSLASH
        // | ch if non_ascii_regex_that_is_ridiculous
        => true,
        _ => false
    }
}

fn is_decimal_digit(ch: char) -> bool {
    match ch {
        '0'...'9' => true,
        _ => false
    }
}

fn octal_digit(ch: char) -> Option<u32> {
    match ch {
        '0' => Some(0),
        '1' => Some(1),
        '2' => Some(2),
        '3' => Some(3),
        '4' => Some(4),
        '5' => Some(5),
        '6' => Some(6),
        '7' => Some(7),
        _ => None
    }
}

fn hex_digit(ch: char) -> Option<u32> {
    match ch {
        '0' => Some(0),
        '1' => Some(1),
        '2' => Some(2),
        '3' => Some(3),
        '4' => Some(4),
        '5' => Some(5),
        '6' => Some(6),
        '7' => Some(7),
        '8' => Some(8),
        '9' => Some(9),
        'a' | 'A' => Some(10),
        'b' | 'B' => Some(11),
        'c' | 'C' => Some(12),
        'd' | 'D' => Some(13),
        'e' | 'E' => Some(14),
        'f' | 'F' => Some(15),
        _ => None
    }
}

fn is_strict_mode_reserved_word(id: &str) -> bool {
    match id {
        "implements" | "interface" | "package" | "private" | "protected" | "public" | "static" |
            "yield" | "let" => true,
        _ => false
    }
}

fn is_strict_mode_reserved<'a>(tok: &T<'a>) -> bool {
    use ast::Tok::*;

    match *tok {
        Implements | Interface | Package | Private | Protected | Public | Static |
            Yield | Let => true,
        _ => false
    }
}

fn is_restricted_word(id: &str) -> bool {
    match id {
        "eval" | "arguments" => true,
        _ => false
    }
}

fn keyword<'a>(id: &str, strict: bool) -> Option<T<'a>> {
    use ast::Tok::*;

    // if strict ...
    Some(match id {
        "if" => If, "in" => In, "do" => Do,
        "var" => Var, "for" => For, "new" => New, "try" => Try, "let" => Let,
        "this" => This, "else" => Else, "case" => Case, "void" => Void, "with" => With, "enum" => Enum,
        "while" => While, "break" => Break, "catch" => Catch, "throw" => Throw, "const" => Const, "yield" => Yield, "class" => Class, "super" => Super,
        "return" => Return, "typeof" => TypeOf, "delete" => Delete, "switch" => Switch, "export" => Export, "import" => Import,
        "default" => Default, "finally" => Finally, "extends" => Extends,
        "function" => Function, "continue" => Continue, "debugger" => Debugger,
        "instanceof" => InstanceOf,

        "implements" if strict => Implements,
        "interface" if strict => Interface,
        "package" if strict => Package,
        "private" if strict => Private,
        "protected" if strict => Protected,
        "public" if strict => Public,
        "static" if strict => Static,
        _ => return None
    })
}

fn assign_op<'a>(tok: &Token<'a>) -> Option<AssignOp> {
    use ast::AssignOp::*;

    Some(match tok.ty {
        T::Eq => Eq,
        T::TimesEq => TimesEq, T::DivEq => DivEq, T::ModEq => ModEq, T::PlusEq => PlusEq,
        T::MinusEq => MinusEq, T::LtLtEq => LtLtEq, T::GtGtEq => GtGtEq, T::GtGtGtEq => GtGtGtEq,
        T::AndEq => AndEq, T::XorEq => XorEq, T::OrEq => OrEq,

        _ => return None
    })
}

fn identifier_name<'a>(tok: &Token<'a>) -> Option<&'a str> {
    use ast::Tok::*;

    Some(match tok.ty {
        T::Identifier(i) => i,

        If => "if", In => "in", Do => "do",
        Var => "var", For => "for", New => "new", Try => "try", Let => "let",
        This => "this", Else => "else", Case => "case", Void => "void", With => "with", Enum => "enum",
        While => "while", Break => "break", Catch => "catch", Throw => "throw", Const => "const", Yield => "yield", Class => "class", Super => "super",
        Return => "return", TypeOf => "typeof", Delete => "delete", Switch => "switch", Export => "export", Import => "import",
        Default => "default", Finally => "finally", Extends => "extends",
        Function => "function", Continue => "continue", Debugger => "debugger",
        InstanceOf => "instanceof",

        Implements => "implements",
        Interface => "interface",
        Package => "package",
        Private => "private",
        Protected => "protected",
        Public => "public",
        Static => "static",

        BooleanLiteral(true) => "true", BooleanLiteral(false) => "false",
        NullLiteral => "null",

        _ => return None
    })
}

type Prec = u8;

fn binop<'a>(tok: &Token<'a>, allow_in: bool) -> Option<(BinOp, Prec)> {
    use ast::BinOp::*;

    Some(match tok.ty {
        T::OrOr => (OrOr, 1),
        T::AndAnd => (AndAnd, 2),
        T::Or => (Or, 3),
        T::Xor => (Xor, 4),
        T::And => (And, 5),
        T::EqEq => (EqEq, 6), T::NEq => (NEq, 6), T::EqEqEq => (EqEqEq, 6), T::NEqEq => (NEqEq, 6),
        T::Lt => (Lt, 7), T::Gt => (Gt, 7), T::LtEq => (LtEq, 7), T::GtEq => (GtEq, 7),
            T::InstanceOf => (InstanceOf, 7),
        T::In if allow_in => (In, 7),
        T::In => return None,
        T::LtLt => (LtLt, 8), T::GtGt => (GtGt, 8), T::GtGtGt => (GtGtGt, 8),
        T::Plus => (Plus, 9), T::Minus => (Minus, 9),
        T::Times => (Times, 11), T::Div => (Div, 11), T::Mod => (Mod, 11),

        _ => return None
    })
}

fn unop<'a>(tok: &Token<'a>) -> Option<UnOp> {
    use ast::UnOp::*;

    Some(match tok.ty {
        T::Delete => Delete,
        T::Void => Void,
        T::TypeOf => TypeOf,
        T::Plus => Plus,
        T::Minus => Minus,
        T::Tilde => Tilde,
        T::Not => Not,

        _ => return None
    })
}

fn updateop<'a>(tok: &Token<'a>) -> Option<UpdateOp> {
    use ast::UpdateOp::*;

    Some(match tok.ty {
        T::PlusPlus => PlusPlus,
        T::MinusMinus => MinusMinus,

        _ => return None
    })
}

enum ScanHex { U, X }

impl<'a> Annotation for () {
    type Ctx = Ctx<'a, (), ()>;
    type Start = ();

    fn start(_: &Ctx<'a, (), ()>) -> () {
        ()
    }

    fn finish(_: &(), _: &Ctx<'a, (), ()>) -> () {
        ()
    }
}

impl<'a, Ann, Start> Ctx<'a, Ann, Start>
    where Ann: Annotation<Ctx=Self, Start=Start>,
{
    #[cold] #[inline(never)]
    fn unexpected_char(&self, opt: Option<(char, &str)>) -> Error<'a> {
        match opt {
            Some((ch, _)) => Error::UnexpectedChar(ch),
            None => Error::UnexpectedEOF,
        }
    }

    fn skip_single_line_comment(&mut self, /*offset*/_: usize) {
        //let start = self.index - offset;
        //let loc =
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.byte_index = self.source.char_range_at(self.byte_index).next;
            self.rest = rest;
            if is_line_terminator(ch) {
                self.has_line_terminator = true;
                // if extra.comments ...
                if ch == '\x0D' {
                    if let Some(('\x0A', rest)) = self.rest.slice_shift_char() {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                    }
                }
                self.line_number += 1;
                self.line_start = self.index;
                return;
            }
        }

        // if extra.comments ...
    }

    fn skip_multi_line_comment(&mut self) -> PRes<'a, ()> {
        // if extra.comments ...

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if is_line_terminator(ch) {
                if ch == '\x0D' {
                    match rest.slice_shift_char() {
                        Some(('\x0A', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            self.rest = rest_;
                        },
                        _ => self.rest = rest
                    }
                } else { self.rest = rest }
                self.has_line_terminator = true;
                self.line_number += 1;
                self.index += 1;
                self.byte_index = self.source.char_range_at(self.byte_index).next;
                self.line_start = self.index;
            } else if ch == '*' {
                // Block comment ends with '*/'.
                if let Some(('/', rest)) = rest.slice_shift_char() {
                    self.index += 2;
                    self.byte_index += 2;
                    self.rest = rest;
                    // if extra.comments ...
                    return Ok(());
                }
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
            } else {
                self.index += 1;
                self.byte_index = self.source.char_range_at(self.byte_index).next;
                self.rest = rest;
            }
        }

        Err(Error::UnexpectedEOF)
    }

    fn skip_comment(&mut self) -> PRes<'a, ()> {
        self.has_line_terminator = false;

        let mut start = self.index == 0;
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if is_white_space(ch) {
                self.index += 1;
                self.byte_index = self.source.char_range_at(self.byte_index).next;
                self.rest = rest;
            } else if is_line_terminator(ch) {
                self.has_line_terminator = true;
                self.index += 1;
                self.byte_index = self.source.char_range_at(self.byte_index).next;
                self.rest = rest;
                if ch == '\x0D' {
                    if let Some(('\x0A', rest)) = self.rest.slice_shift_char() {
                        self.rest = rest;
                        self.index += 1;
                        self.byte_index += 1;
                    }
                }
                self.line_number += 1;
                self.line_start = self.index;
                start = true;
            } else if ch == '/' {
                match rest.slice_shift_char() {
                    Some(('/', rest)) => {
                        self.index += 2;
                        self.byte_index += 2;
                        self.rest = rest;
                        self.skip_single_line_comment(2);
                        start = true;
                    },
                    Some(('*', rest)) => {
                        self.index += 2;
                        self.byte_index += 2;
                        self.rest = rest;
                        try!(self.skip_multi_line_comment())
                    },
                    _ => break
                }
            } else if start && ch == '-' {
                // U+003E is '>'
                match rest.slice_shift_char() {
                    Some(('\x2D', rest)) => match rest.slice_shift_char() {
                        Some(('\x3E', rest)) => {
                            // '-->' is a single-line comment
                            self.index += 3;
                            self.byte_index += 3;
                            self.rest = rest;
                            self.skip_single_line_comment(3);
                        },
                        _ => break
                    },
                    _ => break
                }
            } else if ch == '<' {
                match rest.slice_shift_char() {
                    Some(('!', rest)) => match rest.slice_shift_char() {
                        Some(('-', rest)) => match rest.slice_shift_char() {
                            Some(('-', rest)) => {
                                self.index += 4;
                                self.byte_index += 4;
                                self.rest = rest;
                                self.skip_single_line_comment(4);
                            },
                            _ => break
                        },
                        _ => break
                    },
                    _ => break
                }
            } else {
                break
            }
        }

        Ok(())
    }


    fn scan_hex_escape(&mut self, prefix: ScanHex) -> PRes<'a, u16> {
        let len = match prefix {
            ScanHex::U => 4,
            ScanHex::X => 2,
        };
        let mut i = 0;
        let mut code = 0;
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if i == len { break }
            match hex_digit(ch) {
                Some(i) => {
                    self.index += 1;
                    self.byte_index += 1;
                    self.rest = rest;
                    code = code * 16 + i as u16;
                },
                None => return Err(Error::ExpectedHex)
            }
            i += 1;
        }
        if self.rest.len() == 0 { return Err(Error::UnexpectedEOF) }
        Ok(code)
    }

    fn scan_unicode_code_point_escape(&mut self, s: &mut Vec<u16>) -> PRes<'a, ()> {
        if let Some('}') = self.rest.chars().next() { return Err(Error::UnexpectedChar('}')) }
        let mut code = 0;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.byte_index = self.source.char_range_at(self.byte_index).next;
            self.rest = rest;
            match hex_digit(ch) {
                Some(code_) => code = code * 16 + code_,
                None if ch != '}' => return Err(Error::UnexpectedChar(ch)),
                None => break
            }
            if code > 0x10FFFF { return Err(Error::InvalidUnicode(code)) }
        }
        if code <= 0xFFFF {
            s.push(code as u16);
        } else {
            let cu1 = (((code - 0x10000) >> 10) + 0xD800) as u16;
            let cu2 = (((code - 0x10000) & 1023) + 0xDC00) as u16;
            s.push(cu1);
            s.push(cu2);
        }
        Ok(())
    }

    fn get_escaped_identifier(&mut self, id: &mut String) -> PRes<'a, ()> {
        // '\u' (U+005C, U+0075) denotes an escaped character.
        id.push(match self.rest.slice_shift_char() {
            Some(('u', rest)) => {
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
                match try!(self.scan_hex_escape(ScanHex::U)) as u32 {
                    ch @ UTF16_BACKSLASH | ch if !is_identifier_start(ch) =>
                        return Err(Error::UnexpectedEscape(ch)),
                    ch => match char::from_u32(ch) {
                        Some(ch) => ch,
                        None => return Err(Error::InvalidUnicode(ch))
                    }
                }
            },
            o => return Err(self.unexpected_char(o))
        });

        // '\u' (U+005C, U+0075) denotes an escaped character.
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if !is_identifier_part(ch as u32) { break }
            self.index += 1;
            self.byte_index = self.source.char_range_at(self.byte_index).next;
            self.rest = rest;

            match ch {
                '\\' => match self.rest.slice_shift_char() {
                    Some(('u', rest)) => {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                        id.push(match try!(self.scan_hex_escape(ScanHex::U)) as u32 {
                            ch @ UTF16_BACKSLASH | ch if !is_identifier_part(ch) =>
                                return Err(Error::UnexpectedEscape(ch)),
                            ch => match char::from_u32(ch) {
                                Some(ch) => ch,
                                None => return Err(Error::InvalidUnicode(ch))
                            }
                        });
                    },
                    o => return Err(self.unexpected_char(o))
                },
                _ => id.push(ch)
            }
        }
        Ok(())
    }

    fn get_identifier(&mut self) -> PRes<'a, &'a str> {
        let start_byte_index = self.byte_index;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            match ch {
                '\\' => {
                    // Blackslash (U+005C) marks Unicode escape sequence.
                    let mut id = self.root.ident_arena
                        .alloc(self.source[start_byte_index..self.byte_index].to_string());
                    self.index += 1;
                    self.byte_index += 1;
                    self.rest = rest;
                    return self.get_escaped_identifier(&mut id).and(Ok(id));
                },
                ch if is_identifier_part(ch as u32) => {
                    self.index += 1;
                    self.byte_index = self.source.char_range_at(self.byte_index).next;
                    self.rest = rest;
                },
                _ => break
            }
        }
        Ok(&self.source[start_byte_index..self.byte_index])
    }

    fn scan_identifier(&mut self) -> PRes<'a, Token<'a>> {
        let start = self.index;
        let id = try!(match self.rest.slice_shift_char() {
            Some(('\\', rest)) => {
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
                let mut id = self.root.ident_arena.alloc(String::new());
                self.get_escaped_identifier(id).and(Ok(&**id))
            },
            _ => self.get_identifier()
        });

        let ty = if id.len() == 1 {
            T::Identifier(id)
        } else if let Some(k) = keyword(id, self.strict) {
            k
        } else if id == "null" {
            T::NullLiteral
        } else if id == "true" {
            T::BooleanLiteral(true)
        } else if id == "false" {
            T::BooleanLiteral(false)
        } else {
            T::Identifier(id)
        };

        Ok(Token {
            ty: ty,
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index
        })
    }

    fn scan_punctuator(&mut self) -> PRes<'a, Token<'a>> {
        use ast::Tok::*;

        let line_number = self.line_number;
        let line_start = self.line_start;
        let start = self.index;
        let ty = match self.rest.slice_shift_char() {
            Some((ch, mut rest)) => {
                let ty = match ch {
                    '(' => {
                        // extra.tokenize...
                        LParen
                    },
                    '{' => {
                        // extra.tokenize...
                        LBrack
                    },
                    '.' => match rest.slice_shift_char() {
                        Some(('.', rest_)) => match rest_.slice_shift_char() {
                            Some(('.', rest_)) => {
                                // Spread operator: ...
                                self.index += 2;
                                self.byte_index += 2;
                                rest = rest_;
                                Ellipsis
                            },
                            _ => Dot
                        },
                        _ => Dot,
                    },
                    ')' => RParen,
                    ';' => Semi,
                    ',' => Comma,
                    '}' => RBrack,
                    '[' => LBrace,
                    ']' => RBrace,
                    ':' => Colon,
                    '?' => QMark,
                    '~' => Tilde,
                    '>' => match rest.slice_shift_char() {
                        Some(('>', rest_)) => match rest_.slice_shift_char() {
                            Some(('>', rest_)) => match rest_.slice_shift_char() {
                                Some(('=', rest_)) => {
                                    self.index += 3;
                                    self.byte_index += 3;
                                    rest = rest_;
                                    GtGtGtEq
                                },
                                _ => {
                                    self.index += 2;
                                    self.byte_index += 2;
                                    rest = rest_;
                                    GtGtGt
                                }
                            },
                            Some(('=', rest_)) => {
                                self.index += 2;
                                self.byte_index += 2;
                                rest = rest_;
                                GtGtEq
                            },
                            _ => {
                                self.index += 1;
                                self.byte_index += 1;
                                rest = rest_;
                                GtGt
                            }
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            GtEq
                        },
                        _ => Gt
                    },
                    '=' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => match rest_.slice_shift_char() {
                            Some(('=', rest_)) => {
                                self.index += 2;
                                self.byte_index += 2;
                                rest = rest_;
                                EqEqEq
                            },
                            _ => {
                                self.index += 1;
                                self.byte_index += 1;
                                rest = rest_;
                                EqEq
                            }
                        },
                        Some(('>', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            Arrow
                        },
                        _ => Eq
                    },
                    '!' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => match rest_.slice_shift_char() {
                            Some(('=', rest_)) => {
                                self.index += 2;
                                self.byte_index += 2;
                                rest = rest_;
                                NEqEq
                            },
                            _ => {
                                self.index += 1;
                                self.byte_index += 1;
                                rest = rest_;
                                NEq
                            }
                        },
                        _ => Not
                    },
                    '<' => match rest.slice_shift_char() {
                        Some(('<', rest_)) => match rest_.slice_shift_char() {
                            Some(('=', rest_)) => {
                                self.index += 2;
                                self.byte_index += 2;
                                rest = rest_;
                                LtLtEq
                            },
                            _ => {
                                self.index += 1;
                                self.byte_index += 1;
                                rest = rest_;
                                LtLt
                            }
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            LtEq
                        },
                        _ => Lt
                    },
                    '&' => match rest.slice_shift_char() {
                        Some(('&', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            AndAnd
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            AndEq
                        },
                        _ => And
                    },
                    '|' => match rest.slice_shift_char() {
                        Some(('|', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            OrOr
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            OrEq
                        },
                        _ => Or
                    },
                    '+' => match rest.slice_shift_char() {
                        Some(('+', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            PlusPlus
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            PlusEq
                        },
                        _ => Plus
                    },
                    '-' => match rest.slice_shift_char() {
                        Some(('-', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            MinusMinus
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            MinusEq
                        },
                        _ => Minus
                    },
                    '*' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            TimesEq
                        },
                        _ => Times
                    },
                    '/' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            DivEq
                        },
                        _ => Div
                    },
                    '^' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            XorEq
                        },
                        _ => Xor
                    },
                    '%' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            self.byte_index += 1;
                            rest = rest_;
                            ModEq
                        },
                        _ => Mod
                    },
                    ch => return Err(Error::UnexpectedChar(ch))
                };
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
                ty
            },
            None => return Err(Error::UnexpectedEOF)
        };
        Ok(Token {
            ty: ty,
            line_number: line_number,
            line_start: line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_hex_literal(&mut self, start: Pos) -> PRes<'a, Token<'a>> {
        let byte_begin = self.byte_index;
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if let None = hex_digit(ch) { break }
            self.index += 1;
            self.byte_index += 1;
            self.rest = rest;
        }
        if byte_begin == self.byte_index { return Err(self.unexpected_char(self.rest.slice_shift_char())) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch as u32) { return Err(Error::UnexpectedChar(ch)) }
        }
        Ok(Token {
            ty: T::NumericLiteral(try!(num::from_str_radix(&self.source[byte_begin..self.byte_index], 16))),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_binary_literal(&mut self, start: Pos) -> PRes<'a, Token<'a>> {
        let byte_begin = self.byte_index;
        while let Some(('0'...'1', rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.byte_index += 1;
            self.rest = rest;
        }
        // only 0b or 0B
        if byte_begin == self.byte_index { return Err(self.unexpected_char(self.rest.slice_shift_char())) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch as u32) || is_decimal_digit(ch) { return Err(Error::UnexpectedChar(ch)) }
        }
        Ok(Token {
            ty: T::NumericLiteral(try!(num::from_str_radix(&self.source[byte_begin..self.byte_index], 2))),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_octal_literal<Tag>(&mut self, prefix: char, rest: &'a str, start: Pos,
                          tag: Tag) -> PRes<'a, Token<'a>>
        where Tag: FnOnce(f64) -> T<'a>,
    {
        let octal = match octal_digit(prefix) { Some(_) => true, _ => false };
        let byte_begin = if octal { self.byte_index } else { self.byte_index + 1 };

        self.index += 1;
        self.byte_index += 1;
        self.rest = rest;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if let None = octal_digit(ch) { break }
            self.index += 1;
            self.byte_index += 1;
            self.rest = rest;
        }
        // only 0o or 0O
        if byte_begin == self.byte_index { return Err(self.unexpected_char(self.rest.slice_shift_char())) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch as u32) || is_decimal_digit(ch) { return Err(Error::UnexpectedChar(ch)) }
        }
        Ok(Token {
            ty: tag(try!(num::from_str_radix(&self.source[byte_begin..self.byte_index], 8))),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn is_implicit_octal_literal(&mut self) -> bool {
        for ch in self.rest.chars().skip(1) {
            if let '8'...'9' = ch { return false; }
            if let None = octal_digit(ch) { return true; }
        }
        true
    }

    fn scan_numeric_literal(&mut self, ch: char, rest: &'a str) -> PRes<'a, Token<'a>> {
        let start = self.index;
        let start_byte = self.byte_index;

        if ch != '.' {
            self.index += 1;
            self.byte_index += 1;
            self.rest = rest;

            if ch == '0' {
                match self.rest.slice_shift_char() {
                    Some(('x', rest)) | Some(('X', rest)) => {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                        return self.scan_hex_literal(start);
                    },
                    Some(('b', rest)) | Some(('B', rest)) => {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                        return self.scan_binary_literal(start);
                    },
                    Some((ch @ 'o', rest)) | Some((ch @ 'O', rest)) => {
                        return self.scan_octal_literal(ch, rest, start, T::NumericLiteral);
                    },
                    Some((ch, rest)) => if let Some(_) = octal_digit(ch) {
                        if self.is_implicit_octal_literal() {
                            return self.scan_octal_literal(ch, rest, start, T::OctalIntegerLiteral);
                        }
                    },
                    _ => {}
                }
            }

            while let Some((ch, rest)) = self.rest.slice_shift_char() {
                if !is_decimal_digit(ch) { break }
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
            }
        }

        if let Some(('.', rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.byte_index += 1;
            self.rest = rest;

            while let Some((ch, rest)) = self.rest.slice_shift_char() {
                if !is_decimal_digit(ch) { break }
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
            }
        }

        match self.rest.slice_shift_char() {
            Some(('e', rest)) | Some(('E', rest)) => {
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
                match self.rest.slice_shift_char() {
                    Some(('+', rest)) | Some(('-', rest)) => {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                    },
                    _ => {}
                }
                if let Some((ch, rest)) = self.rest.slice_shift_char() {
                    if is_decimal_digit(ch) {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                        while let Some((ch, rest)) = self.rest.slice_shift_char() {
                            if !is_decimal_digit(ch) { break }
                            self.index += 1;
                            self.byte_index += 1;
                            self.rest = rest;
                        }
                    } else { return Err(Error::UnexpectedChar(ch)); }
                }
            },
            _ => {}
        }

        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch as u32) { return Err(Error::UnexpectedChar(ch)) }
        }

        Ok(Token {
            ty: T::NumericLiteral(try!(self.source[start_byte..self.byte_index].parse())),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    #[inline]
    fn scan_escaped_string_literal(&mut self, quote: char, start: Pos, s: &'a mut Vec<u16>) -> PRes<'a, Token<'a>>
    {
        let mut octal = false;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.byte_index = self.source.char_range_at(self.byte_index).next;
            self.rest = rest;

            if ch == quote { break }
            else if ch == '\\' {
                match self.rest.slice_shift_char() {
                    Some((ch, rest)) => {
                        self.index += 1;
                        self.byte_index = self.source.char_range_at(self.byte_index).next;
                        self.rest = rest;
                        if !is_line_terminator(ch) {
                            match ch {
                                'u' | 'x' => {
                                    match self.rest.slice_shift_char() {
                                        Some(('{', rest)) => {
                                            self.index += 1;
                                            self.byte_index += 1;
                                            self.rest = rest;
                                            try!(self.scan_unicode_code_point_escape(s));
                                        },
                                        _ => {
                                            let restore = self.index;
                                            let byte_restore = self.byte_index;
                                            let prefix = if ch == 'u' { ScanHex::U } else { ScanHex::X };
                                            match self.scan_hex_escape(prefix) {
                                                Ok(c) => s.push(c),
                                                _ => {
                                                    self.index = restore;
                                                    self.byte_index = byte_restore;
                                                    self.rest = rest;
                                                    s.extend(Utf16Encoder::new(Some(ch).into_iter()));
                                                }
                                            }
                                        }
                                    }
                                },
                                'n' => s.push('\n' as u16),
                                'r' => s.push('\r' as u16),
                                't' => s.push('\t' as u16),
                                'b' => s.push('\x08' as u16),
                                'f' => s.push('\x0C' as u16),
                                'v' => s.push('\x0B' as u16),
                                _ => match octal_digit(ch) {
                                    Some(mut code) => {
                                        // \0 is not octal escape sequence
                                        if code != 0 { octal = true; }

                                        if let Some((ch_, rest)) = self.rest.slice_shift_char() {
                                            if let Some(code_) = octal_digit(ch_) {
                                                octal = true;
                                                code = code * 8 + code_;
                                                self.index += 1;
                                                self.byte_index += 1;
                                                self.rest = rest;

                                                // 3 digits are only allowed when string starts
                                                // with 0, 1, 2, 3
                                                if let '0'...'3' = ch {
                                                    if let Some((ch_, rest)) = self.rest.slice_shift_char() {
                                                        if let Some(code_) = octal_digit(ch_) {
                                                            self.index += 1;
                                                            self.byte_index += 1;
                                                            self.rest = rest;
                                                            code = code * 8 + code_;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        s.push(code as u16);
                                    },
                                    None => s.extend(Utf16Encoder::new(Some(ch).into_iter()))
                                }
                            }
                        } else {
                            self.line_number += 1;
                            if ch == '\r' {
                                if let Some(('\n', rest)) = self.rest.slice_shift_char() {
                                    self.index += 1;
                                    self.byte_index += 1;
                                    self.rest = rest;
                                }
                            }
                            self.line_start = self.index;
                        }
                    },
                    None => return Err(Error::UnterminatedStringLiteral)
                }
            } else if is_line_terminator(ch) {
                return Err(Error::UnexpectedChar(ch));
            } else {
                s.extend(Utf16Encoder::new(Some(ch).into_iter()))
            }
        }
        if self.rest.len() == 0 { return Err(Error::UnexpectedEOF) }
        Ok(Token {
            ty: if octal { T::OctalStringLiteral(s) } else { T::EscapedStringLiteral(s) },
            line_number: self.start_line_number,
            line_start: self.start_line_start,
            start: start,
            end: self.index
        })
    }

    // 7.8.4 String Literals

    fn scan_string_literal(&mut self, quote: char) -> PRes<'a, Token<'a>>
    {
        debug_assert!(quote == '"' || quote == '\'', r#"Quote character must be `'` or `"`."#);

        let start = self.index - 1;
        let byte_start = self.byte_index - 1;

        loop {
            match self.rest.slice_shift_char() {
                Some((ch, rest)) => {
                    if ch == quote {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                        break
                    }
                    else if ch == '\\' {
                        let s = self.root.string_arena
                            .alloc(Utf16Encoder::new(self.source[byte_start + 1..self.byte_index].chars())
                                   .collect());
                        return self.scan_escaped_string_literal(quote, start, s);
                    } else {
                        self.index += 1;
                        self.byte_index = self.source.char_range_at(self.byte_index).next;
                        self.rest = rest;
                        if is_line_terminator(ch) { return Err(Error::UnexpectedChar(ch)); }
                    }
                },
                None => { return Err(Error::UnexpectedEOF) }
            }
        }
        Ok(Token {
            ty: T::StringLiteral(&self.source[byte_start + 1..self.byte_index - 1]),
            line_number: self.start_line_number,
            line_start: self.start_line_start,
            start: start,
            end: self.index
        })
    }

    fn scan_reg_exp_body(&mut self) -> PRes<'a, ()> {
        // In theory, this shouldn't actually ever be false... I guess... I might go over the
        // algorithm later to see if I can eliminate this.
        let (ch, rest) = self.rest.slice_shift_char()
                             .expect("Regular expression literal must start with a slash");
        debug_assert!(ch == '/', "Regular expression literal must start with a slash");
        self.index += 1;
        self.byte_index += 1;
        self.rest = rest;

        let mut class_marker = false;
        let mut terminated = false;
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.byte_index = self.source.char_range_at(self.byte_index).next;
            self.rest = rest;
            // add ch to str

            match ch {
                '\\' => {
                    match self.rest.slice_shift_char() {
                        Some((ch, rest)) => {
                            // In the Esprima version this actually gets incremented even in the
                            // None state, but that seems like it would give bad error locations.
                            self.index += 1;
                            self.byte_index = self.source.char_range_at(self.byte_index).next;
                            self.rest = rest;
                            if is_line_terminator(ch) { return Err(Error::UnterminatedRegExp) }
                            // add ch to str
                        },
                        None => return Err(Error::UnterminatedRegExp)
                    }
                },
                ch if is_line_terminator(ch) => return Err(Error::UnterminatedRegExp),
                ']' if class_marker => { class_marker = false },
                '/' if !class_marker => { terminated = true; break },
                '[' if !class_marker => { class_marker = true },
                _ => {}
            }
        }

        if terminated {
            //let body = ...
            Ok(())
        } else {
            Err(Error::UnterminatedRegExp)
        }
    }

    fn scan_reg_exp_flags(&mut self) -> PRes<'a, ()> {
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if !is_identifier_part(ch as u32) { break }

            self.index += 1;
            self.byte_index = self.source.char_range_at(self.byte_index).next;
            self.rest = rest;
            match ch {
                '\\' => match self.rest.slice_shift_char() {
                    Some(('u', rest)) => {
                        self.index += 1;
                        self.byte_index += 1;
                        self.rest = rest;
                        let restore = self.index;
                        let byte_restore = self.byte_index;
                        match self.scan_hex_escape(ScanHex::U) {
                            Ok(_c) => {
                                // flags += ch
                                // for str = '\\u'; restore < index; ++restore {
                                //   str += source[restore]
                                // }
                            },
                            _ => {
                                self.index = restore;
                                self.byte_index = byte_restore;
                                self.rest = rest;
                                // self.flags += 'u'
                                // str += "\\u"
                            }
                        }
                        // tolerate
                        return Err(Error::UnexpectedChar(ch))
                    },
                    Some((ch, _)) => {
                        // str += '\\'
                        // tolerate
                        return Err(Error::UnexpectedChar(ch))
                    },
                    None => {
                        // flags += ch
                        // str += ch
                    }
                },
                _ => {
                    // flags += ch
                    // str += ch
                }
            }
        }

        Ok(())
    }

    fn scan_reg_exp(&mut self) -> PRes<'a, RegExp> {
        self.scanning = true;

        self.lookahead = None;
        try!(self.skip_comment());
        //let start = self.index;
        //let byte_start = self.byte_index;

        let _body = try!(self.scan_reg_exp_body());
        let _flags = try!(self.scan_reg_exp_flags());
        // let value = try!(self.test_reg_exp(body, flags));
        self.scanning = false;
        // if extra.tokenize ...

        Ok(RegExp/* {
            literal: body.literal + flags.literal,
            value: value,
            pattern: body,
            flags: flags,
            start: start,
            end: self.index,
        }*/)
    }

    fn advance(&mut self) -> PRes<'a, Token<'a>> {
        match self.rest.slice_shift_char() {
            None => Ok(Token {
                ty: T::EOF,
                line_number: self.line_number,
                line_start: self.line_start,
                start: self.index,
                end: self.index,
            }),
            Some((ch, _)) if is_identifier_start(ch as u32) => self.scan_identifier(),
            Some(('(', _)) | Some((')', _)) | Some((';', _)) => self.scan_punctuator(),
            Some((ch @ '\'', rest)) | Some((ch @ '"', rest)) => {
                self.index += 1;
                self.byte_index += 1;
                self.rest = rest;
                self.scan_string_literal(ch)
            },
            Some(('.', rest)) => match rest.slice_shift_char() {
                Some((ch, _)) if is_decimal_digit(ch) => self.scan_numeric_literal('.', rest),
                _ => self.scan_punctuator()
            },
            Some((ch, rest)) if is_decimal_digit(ch) => self.scan_numeric_literal(ch, rest),
            // if extra.tokenize ...
            _ => self.scan_punctuator()
        }
    }

    fn lex(&mut self) -> PRes<'a, /*Token<'a>*/()> {
        self.scanning = true;

        self.last_index = Some(self.index);
        self.last_line_number = Some(self.line_number);
        self.last_line_start = Some(self.line_start);

        try!(self.skip_comment());

        // let token = self.lookahead;

        self.start_index = self.index;
        self.start_byte_index = self.byte_index;
        self.start_line_number = self.line_number;
        self.start_line_start = self.line_start;

        self.lookahead = match try!(self.advance()) {
            Token { ty: T::EOF, .. } => None,
            tok => Some(/* extra.tokens*/tok)
        };
        //println!("{:?}", self.lookahead);

        self.scanning = false;

        // token
        Ok(())
    }

    fn peek(&mut self) -> PRes<'a, ()> {
        self.scanning = true;

        try!(self.skip_comment());

        self.last_index = Some(self.index);
        self.last_line_number = Some(self.line_number);
        self.last_line_start = Some(self.line_start);

        self.start_index = self.index;
        self.start_byte_index = self.byte_index;
        self.start_line_number = self.line_number;
        self.start_line_start = self.line_start;

        self.lookahead = match try!(self.advance()) {
            Token { ty: T::EOF, .. } => None,
            tok => Some(/* extra.tokens */tok)
        };
        //println!("{:?}", self.lookahead);

        self.scanning = false;

        Ok(())
    }

}

struct Params<'a, Ann> {
    params: Vec<IN<'a, Ann>>,
    defaults: Vec<BEN<'a, Ann>>,
    rest: Option<IN<'a, Ann>>,
    stricted: Option<Error<'a>>,
    first_restricted: Option<Error<'a>>,
}

enum InitFor<'a, Ann> {
    Var(VDN<'a, Ann>),
    Exp(EN<'a, Ann>),
}

impl<'a, Ann, Start> Ctx<'a, Ann, Start>
    where Ann: Annotation<Ctx=Self, Start=Start>,
          //Ann: fmt::Debug,
{
    #[cold] #[inline(never)]
    fn unexpected_token(&self, opt: Option<Token<'a>>) -> Error<'a> {
        match opt {
            Some(tok) => Error::UnexpectedToken(tok),
            None => Error::UnexpectedEOF,
        }
    }

    fn start(&self) -> <Ann as Annotation>::Start {
        <Ann as Annotation>::start(self)
    }

    fn expect_comma_separator(&mut self) -> PRes<'a, ()> {
        // if extra.errors ...
        Ok(expect!(self, T::Comma))
    }

    fn consume_semicolon(&mut self) -> PRes<'a, ()> {
        // Catch the common case first...
        // if Some(';') == self.rest.chars().next() { try!(self.lex()); return Ok(()) }
        if let tk!(T::Semi) = self.lookahead { try!(self.lex()); return Ok(()) }

        if self.has_line_terminator { return Ok(()) }

        self.last_index = Some(self.start_index);
        self.last_line_number = Some(self.start_line_number);
        self.last_line_start = Some(self.start_line_start);

        match self.lookahead {
            tk!(T::RBrack) | None => Ok(()),
            o => Err(self.unexpected_token(o))
        }
    }

    // 11.1.4 Array Initializer

    fn parse_array_initializer(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let mut elements = Vec::new();
        let node = self.start();

        expect!(self, T::LBrace);

        loop {
            match self.lookahead {
                tk!(T::RBrace) => break,
                tk!(T::Comma) => {
                    try!(self.lex());
                    elements.push(None);
                },
                _ => {
                    elements.push(Some(try!(self.parse_assignment_expression())));
                    if !tmatch!(self.lookahead, T::RBrace) {
                        expect!(self, T::Comma);
                    }
                }
            }
        }

        try!(self.lex());

        Ok(finish(&node, self, E::Array(elements)))
    }

    // 11.1.5 Object Initialiser

    fn parse_object_property_key(&mut self) -> PRes<'a, PN<'a, Ann>> {
        let node = self.start();

        let token = self.lookahead;
        try!(self.lex());

        let property = match token {
            Some(tok @ Token { ty: T::OctalStringLiteral(_), .. }) |
            Some(tok @ Token { ty: T::OctalIntegerLiteral(_), .. }) if self.strict => {
                // tolerate
                return Err(Error::StrictOctalLiteral(tok));
            },
            tk!(T::StringLiteral(s)) => Property::String(s),
            tk!(T::EscapedStringLiteral(l)) => Property::EscapedString(l),
            tk!(T::OctalStringLiteral(l)) => Property::EscapedString(l),
            tk!(T::NumericLiteral(n)) => Property::Numeric(n),
            tk!(T::OctalIntegerLiteral(n)) => Property::Numeric(n),
            tk!(T::LBrace) => {
                let expr = try!(self.parse_assignment_expression());
                expect!(self, T::RBrace);
                Property::Computed(expr)
            },
            Some(token) => match identifier_name(&token) {
                Some(name) => Property::Identifier(name),
                None => return Err(Error::UnexpectedToken(token))
            },
            None => return Err(Error::UnexpectedEOF),
        };

        Ok(finish(&node, self, property))
    }


    fn parse_object_property(&mut self, /*has_proto*/_: &mut bool) -> PRes<'a, PDN<'a, Ann>> {
        //let token = self.lookahead;
        let node = self.start();

        let key = try!(self.parse_object_property_key());
        /*let maybe_method = try!(self.try_parse_method_definition(token, key, computed, node));

        if let Some(property) = maybe_method {
            try!(self.check_proto(maybe_method.key, maybe_method.computed, has_proto));
            // finished
            return Ok(maybe_method)
        }

        // init property or short hand property
        try!(self.check_proto(key, computed, has_proto));*/

        if let tk!(T::Colon) = self.lookahead {
            try!(self.lex());
            let value = try!(self.parse_assignment_expression());
            return Ok(finish(&node, self, PropertyDefinition::Property(key, value)));
        }

        Err(self.unexpected_token(self.lookahead))
    }

    fn parse_object_initializer(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let mut properties = Vec::new();
        let mut has_proto = false;
        let node = self.start();

        expect!(self, T::LBrack);

        loop {
            match self.lookahead {
                tk!(T::RBrack) => break,
                _ => {
                    properties.push(try!(self.parse_object_property(&mut has_proto)));

                    if !tmatch!(self.lookahead, T::RBrack) {
                        try!(self.expect_comma_separator())
                    }
                }
            }
        }

        expect!(self, T::RBrack);

        Ok(finish(&node, self, E::Object(properties)))
    }

    // 11.1.6 The Grouping Operator
    fn parse_group_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        expect!(self, T::LParen);

        self.state.parenthesis_count += 1;

        let expr = try!(self.parse_expression());

        expect!(self, T::RParen);

        Ok(expr)
    }

    // 11.1 Primary Expressions

    fn parse_primary_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let node = self.start();
        if let Some(token) = self.lookahead {
            let exp = match token.ty {
                T::LParen => return self.parse_group_expression(),
                T::LBrace => return self.parse_array_initializer(),
                T::LBrack => return self.parse_object_initializer(),
                T::This => { try!(self.lex()); E::This },
                T::Identifier(v) => { try!(self.lex()); E::Identifier(v) },
                T::StringLiteral(s) => { try!(self.lex()); E::String(s) },
                T::EscapedStringLiteral(s) => { try!(self.lex()); E::EscapedString(s) },
                T::OctalStringLiteral(o) => {
                    if self.strict {
                        // tolerate
                        return Err(Error::StrictOctalLiteral(token))
                    }
                    try!(self.lex());
                    E::EscapedString(o)
                },
                T::NumericLiteral(n) => { try!(self.lex()); E::Number(n) },
                T::OctalIntegerLiteral(n) => {
                    if self.strict {
                        // tolerate
                        return Err(Error::StrictOctalLiteral(token))
                    }
                    try!(self.lex());
                    E::Number(n)
                },
                T::Function => return self.parse_function_expression(node),
                // T::Class => return self.parse_class_expression(node),
                T::Div | T::DivEq => {
                    self.index = self.start_index;
                    self.byte_index = self.start_byte_index;
                    self.rest = &self.source[self.byte_index..];
                    // if extra.tokens ...
                    let token = try!(self.scan_reg_exp());
                    try!(self.lex());
                    E::RegExp(token)
                },
                T::BooleanLiteral(b) => { try!(self.lex()); E::Bool(b) },
                T::NullLiteral => { try!(self.lex()); E::Null },
                _ => {
                    try!(self.lex());
                    return Err(Error::UnexpectedToken(token))
                }
            };
            Ok(finish(&node, self, exp))
        } else {
            return Err(Error::UnexpectedEOF)
        }
    }

    // 11.2 Left-Hand-Side Expressions


    fn parse_arguments(&mut self) -> PRes<'a, Vec<EN<'a, Ann>>> {
        let mut args = Vec::new();

        expect!(self, T::LParen);

        if !tmatch!(self.lookahead, T::RParen) {
            while let Some(_) = self.lookahead {
                args.push(try!(self.parse_assignment_expression()));
                if let tk!(T::RParen) = self.lookahead { break }

                try!(self.expect_comma_separator());
            }
        }

        expect!(self, T::RParen);

        Ok(args)
    }

    fn parse_non_computed_property(&mut self) -> PRes<'a, PN<'a, Ann>> {
        let node = self.start();

        let token = self.lookahead;
        try!(self.lex());

        match token.as_ref().and_then(identifier_name) {
            Some(name) => Ok(finish(&node, self, Property::Identifier(name))),
            None => Err(self.unexpected_token(token))
        }
    }

    fn parse_non_computed_member(&mut self) -> PRes<'a, PN<'a, Ann>> {
        expect!(self, T::Dot);

        self.parse_non_computed_property()
    }

    fn parse_computed_member(&mut self) -> PRes<'a, PN<'a, Ann>> {
        expect!(self, T::LBrace);

        let node = self.start();
        let expr = try!(self.parse_expression());
        let node = finish(&node, self, Property::Computed(expr));

        expect!(self, T::RBrace);

        Ok(node)
    }

    fn parse_new_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let node = self.start();

        expect!(self, T::New);
        let callee = try!(self.parse_left_hand_side_expression());
        let args = match self.lookahead {
            tk!(T::LParen) => try!(self.parse_arguments()),
            _ => Vec::new()
        };

        Ok(finish(&node, self, E::New(Box::new(callee), args)))
    }

    fn parse_left_hand_side_expression_allow_call(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let previous_allow_in = self.state.allow_in;

        let node = self.start();
        self.state.allow_in = true;
        let mut expr = try!(match self.lookahead {
            tk!(T::New) => self.parse_new_expression(),
            _ => self.parse_primary_expression()
        });

        loop {
            expr = {
                let expr = match self.lookahead {
                    tk!(T::Dot) => {
                        let property = try!(self.parse_non_computed_member());
                        E::Member(Box::new((expr, property)))
                    },
                    tk!(T::LParen) => {
                        let args = try!(self.parse_arguments());
                        E::Call(Box::new(expr), args)
                    },
                    tk!(T::LBrace) => {
                        let property = try!(self.parse_computed_member());
                        E::Member(Box::new((expr, property)))
                    }
                    _ => { break }
                };
                finish(&node, self, expr)
            }
        }
        self.state.allow_in = previous_allow_in;

        Ok(expr)
    }

    fn parse_left_hand_side_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let node = self.start();

        let mut expr = try!(match self.lookahead {
            tk!(T::New) => self.parse_new_expression(),
            _ => self.parse_primary_expression()
        });

        loop {
            expr = {
                let expr = match self.lookahead {
                    tk!(T::LBrace) => {
                        let property = try!(self.parse_computed_member());
                        E::Member(Box::new((expr, property)))
                    },
                    tk!(T::LParen) => {
                        let args = try!(self.parse_arguments());
                        E::Call(Box::new(expr), args)
                    },
                    _ => { break }
                };
                finish(&node, self, expr)
            }
        }

        Ok(expr)
    }

    // 11.3 Postfix Expressions

    fn parse_postfix_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let node = self.start();

        let expr = try!(self.parse_left_hand_side_expression_allow_call());

        match self.lookahead.as_ref().and_then(updateop) {
            Some(op) if !self.has_line_terminator => {
                // 11.3.1, 11.3.2
                match expr {
                    A(_, E::Identifier(i)) if self.strict && is_restricted_word(&i) => {
                        // tolerate
                        Err(Error::StrictLHSPostfix)
                    },
                    A(_, E::Identifier(_)) | A(_, E::Member(_)) => {
                        try!(self.lex());
                        Ok(finish(&node, self, E::Update(op, UpdateType::Postfix, Box::new(expr))))
                    },
                    _ => {
                        // tolerate
                        Err(Error::InvalidLHSInAssignment)
                    }
                }
            },
            _ => Ok(expr)
        }
    }

    // 11.4 Unary Operators

    fn parse_unary_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        use ast::UnOp::*;

        match self.lookahead.as_ref().and_then(unop) {
            Some(op @ Plus) | Some(op @ Minus) | Some(op @ Tilde) | Some(op @ Not) |
            Some(op @ Void) | Some(op @ TypeOf) => {
                let node = self.start();
                try!(self.lex());
                let expr = try!(self.parse_unary_expression());
                Ok(finish(&node, self, E::Unary(op, Box::new(expr))))
            },
            Some(Delete) => {
                let node = self.start();
                try!(self.lex());
                match try!(self.parse_unary_expression()) {
                    A(_, E::Identifier(_)) if self.strict => {
                        // tolerate
                        Err(Error::StrictDelete)
                    },
                    expr => Ok(finish(&node, self, E::Unary(Delete, Box::new(expr))))
                }
            },
            None => match self.lookahead.as_ref().and_then(updateop) {
                Some(op) => {
                    let node = self.start();
                    try!(self.lex());
                    let expr = try!(self.parse_unary_expression());
                    // 11.4.4, 11.4.5
                    match expr {
                        A(_, E::Identifier(i)) if self.strict && is_restricted_word(&i) => {
                            // tolerate
                            Err(Error::StrictLHSPrefix)
                        },
                        A(_, E::Identifier(_)) | A(_, E::Member(_)) => {
                            Ok(finish(&node, self, E::Update(op, UpdateType::Prefix, Box::new(expr))))
                        },
                        _ => {
                            // tolerate
                            Err(Error::InvalidLHSInAssignment)
                        }
                    }
                },
                None => self.parse_postfix_expression()
            }
        }
    }

    // 11.5 Multiplicative Operators
    // 11.6 Additive Operators
    // 11.7 Bitwise Shift Operators
    // 11.8 Relational Operators
    // 11.9 Equality Operators
    // 11.10 Binary Bitwise Operators
    // 11.11 Binary Logical Operators

    #[inline]
    fn shift_reduce_binary_expression(&mut self,
                                      mut bot: EN<'a, Ann>,
                                      bot_marker: Ann::Start,
                                      bot_index: usize,
                                     ) -> PRes<'a, EN<'a, Ann>> {
        while let Some((op, prec)) = self.lookahead.and_then( |ref t| binop(t, self.state.allow_in) ) {
            // Reduce: make a binary expression from the three topmost entries.
            while let Some((op_, prec_, right_, right_marker_)) = self.binop_stack.pop() {
                if self.binop_stack.len() <= bot_index || prec > prec_ {
                    self.binop_stack.push((op_, prec_, right_, right_marker_));
                    break
                }
                match self.binop_stack.pop() {
                    Some((op__, prec__, left_, left_marker_)) => {
                        if self.binop_stack.len() <= bot_index {
                            // Stack bottom
                            self.binop_stack.push((op__, prec__, left_, left_marker_));
                            bot = finish(&bot_marker, self, E::Binary(op_, Box::new((bot, right_))));
                        } else {
                            let expr = finish(&left_marker_, self, E::Binary(op_, Box::new((left_, right_))));
                            self.binop_stack.push((op__, prec__, expr, left_marker_));
                        }
                    },
                    None => {
                        // Stack bottom
                        bot = finish(&bot_marker, self, E::Binary(op_, Box::new((bot, right_))));
                    }
                }
            }

            // Shift.
            try!(self.lex());
            let marker = self.start();
            let expr = try!(self.parse_unary_expression());
            self.binop_stack.push((op, prec, expr, marker));
        }

        // Final reduce to clean-up the stack.
        Ok(if self.binop_stack.len() > bot_index {
            if let Some((mut op, _, mut expr, _)) = self.binop_stack.pop() {
                while let Some((op_, prec_, expr_, marker_)) = self.binop_stack.pop() {
                    if self.binop_stack.len() <= bot_index {
                        // Stack bottom
                        self.binop_stack.push((op_, prec_, expr_, marker_));
                        break
                    }
                    expr = finish(&marker_, self, E::Binary(op, Box::new((expr_, expr))));
                    op = op_;
                }
                // Stack bottom
                finish(&bot_marker, self, E::Binary(op, Box::new((bot, expr))))
            } else { bot }
        } else { bot })
    }

    fn parse_binary_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let left_marker = self.start();
        let left = try!(self.parse_unary_expression());

        // if arrow .... return left

        let (op, prec) = match self.lookahead.and_then( |ref t| binop(t, self.state.allow_in) ) {
            Some(op) => op,
            None => return Ok(left)
        };
        try!(self.lex());

        let right_marker = self.start();
        let right = try!(self.parse_unary_expression());

        let bottom = self.binop_stack.len();

        self.binop_stack.push((op, prec, right, right_marker));

        let expr = self.shift_reduce_binary_expression(left, left_marker, bottom);
        if self.binop_stack.len() > bottom {
            // Important that truncate be called before returning, to keep the stack clean.
            self.binop_stack.truncate(bottom);
        }

        expr
    }

    // 11.12 Conditional Operator

    fn parse_conditional_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let node = self.start();

        let expr = try!(self.parse_binary_expression());
        // if arrows ...
        Ok(match self.lookahead {
            tk!(T::QMark) => {
                try!(self.lex());
                let previous_allow_in = self.state.allow_in;
                self.state.allow_in = true;
                let consequent = try!(self.parse_assignment_expression());
                // NOTE: Should we make sure this runs on error (above)?
                self.state.allow_in = previous_allow_in;
                expect!(self, T::Colon);
                let alternate = try!(self.parse_assignment_expression());

                finish(&node, self, E::Conditional(Box::new((expr, consequent, alternate))))
            },
            _ => expr
        })
    }

    fn parse_assignment_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let node = self.start();
        let token = match self.lookahead {
            Some(token) => token,
            None => return Err(Error::UnexpectedEOF)
        };

        let expr = try!(self.parse_conditional_expression());

        // Arrow

        match self.lookahead.as_ref().and_then(assign_op) {
            Some(op) => {
                match expr {
                    A(_, E::Identifier(i)) if self.strict && is_restricted_word(&i) => {
                        // tolerate
                        Err(Error::StrictLHSAssignment(token))
                    },
                    A(_, E::Identifier(_)) | A(_, E::Member(_)) => {
                        try!(self.lex());
                        let right = try!(self.parse_assignment_expression());
                        Ok(finish(&node, self, E::Assignment(op, Box::new((expr, right)))))
                    },
                    _ => {
                        // tolerate
                        Err(Error::InvalidLHSInArgument)
                    }
                }
            },
            None => Ok(expr)
        }
    }

    // 11.14 Comma Operator

    fn parse_expression(&mut self) -> PRes<'a, EN<'a, Ann>> {
        let node = self.start();

        let expr = try!(self.parse_assignment_expression());

        match self.lookahead {
            tk!(T::Comma) => {
                let mut expressions = vec![expr];

                while let Some(tok) = self.lookahead {
                    match tok.ty {
                        T::Comma => {
                            try!(self.lex());
                            expressions.push(try!(self.parse_assignment_expression()));
                        }
                        _ => break
                    }
                }

                /*let res = */Ok(finish(&node, self, E::Seq(expressions)))/*;
                println!("Parsed expression: {:?}", res);
                res*/
            },
            _ => { /*println!("Parsed expression: {:?}", expr); */ Ok(expr) }
        }
    }

    fn parse_statement_list(&mut self) -> PRes<'a, Block<'a, Ann>> {
        let mut list = Vec::new();
        while let Some(token) = self.lookahead {
            if let T::RBrack = token.ty { break }
            list.push(try!(self.parse_statement_list_item()));
        }

        Ok(list)
    }

    fn parse_block(&mut self) -> PRes<'a, Block<'a, Ann>> {
        expect!(self, T::LBrack);

        let block = try!(self.parse_statement_list());

        expect!(self, T::RBrack);

        Ok(block)
    }

    fn parse_variable_identifier(&mut self) -> PRes<'a, IN<'a, Ann>> {
        let node = self.start();
        let token = self.lookahead;
        try!(self.lex());
        match token {
            tk!(T::Identifier(i)) => Ok(finish(&node, self, i)),
            Some(token) => {
                if self.strict && is_strict_mode_reserved(&token.ty) {
                    // tolerate
                    Err(Error::StrictReservedWord(token))
                } else {
                    Err(Error::UnexpectedToken(token))
                }
            },
            None => Err(Error::UnexpectedEOF)
        }
    }

    fn parse_variable_declaration(&mut self) -> PRes<'a, BEN<'a, Ann>> {
        let node = self.start();

        let id = try!(self.parse_variable_identifier());

        // 12.2.1
        if self.strict && is_restricted_word(&id) {
            // tolerate
            return Err(Error::StrictVarName)
        }

        let binding = match self.lookahead {
            tk!(T::Eq) => {
                try!(self.lex());
                let init = try!(self.parse_assignment_expression());
                BindingElement::SingleName(id, init)
            },
            _ => BindingElement::Uninitialized(id)
        };

        Ok(finish(&node, self, binding))
    }

    fn parse_variable_declaration_list(&mut self) -> PRes<'a, Vec<BEN<'a, Ann>>> {
        let mut list = Vec::new();

        loop {
            list.push(try!(self.parse_variable_declaration()));
            match self.lookahead {
                tk!(T::Comma) => {
                    try!(self.lex());
                    if let None = self.lookahead { break }
                }
                _ => break
            }
        }

        Ok(list)
    }

    fn parse_variable_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Var);

        let declarations = try!(self.parse_variable_declaration_list());

        try!(self.consume_semicolon());

        Ok(finish(&node, self, Statement::Variable(declarations)))
    }

    fn parse_empty_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Semi);
        Ok(finish(&node, self, Statement::Empty))
    }

    // NOTE: This seems to be redundant.
    // 12.4 Expression Statement

    /*fn parse_expression_statement<Ann>(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = try!(self.parse_expression());
        expect!(self, T::Semi);
        Ok(finish(node, self, Statement::Expression(expr)))
    }*/

    // 12.5 If statement

    fn parse_if_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::If);

        expect!(self, T::LParen);

        let test = try!(self.parse_expression());

        expect!(self, T::RParen);

        let consequent = try!(self.parse_statement());

        let alternate = match self.lookahead {
            tk!(T::Else) => { try!(self.lex()); Some(Box::new(try!(self.parse_statement()))) },
            _ => None
        };

        Ok(finish(&node, self, Statement::If(test, Box::new(consequent), alternate)))
    }

    // 12.6 Iteration Statements

    fn parse_do_while_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Do);

        let old_in_iteration = self.state.in_iteration;
        self.state.in_iteration = true;

        let body = try!(self.parse_statement());

        // NOTE: Should this be done even if the above fails?
        self.state.in_iteration = old_in_iteration;

        expect!(self, T::While);

        expect!(self, T::LParen);

        let test = try!(self.parse_expression());

        expect!(self, T::RParen);

        if let tk!(T::Semi) = self.lookahead { try!(self.lex()); }

        Ok(finish(&node, self, Statement::DoWhile(Box::new(body), test)))
    }

    fn parse_while_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::While);

        expect!(self, T::LParen);

        let test = try!(self.parse_expression());

        expect!(self, T::RParen);

        let old_in_iteration = self.state.in_iteration;
        self.state.in_iteration = true;

        let body = try!(self.parse_statement());

        // NOTE: Should this be done even if the above fails?
        self.state.in_iteration = old_in_iteration;

        Ok(finish(&node, self, Statement::While(test, Box::new(body))))
    }

    fn parse_for_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        let previous_allow_in = self.state.allow_in;
        let mut inexp = None;

        let mut init = None;
        let mut test = None;
        let mut update = None;

        expect!(self, T::For);

        expect!(self, T::LParen);

        match self.lookahead {
            tk!(T::Semi) => try!(self.lex()),
            tk!(T::Var) => {
                let init_ = self.start();
                try!(self.lex());

                self.state.allow_in = false;
                let declarations = try!(self.parse_variable_declaration_list());
                let init_ = finish(&init_, self, VariableDeclaration {
                    declarations: declarations,
                    kind: VariableDeclarationKind::Var
                });
                // NOTE: Should we make sure this happens even if the above fails?
                self.state.allow_in = previous_allow_in;

                match self.lookahead {
                    tk!(T::In) if init_.declarations.len() == 1 => {
                        try!(self.lex());
                        let right = try!(self.parse_expression());
                        inexp = Some((InitFor::Var(init_), right));
                    },
                    _ => {
                        init = Some(InitFor::Var(init_));
                        expect!(self, T::Semi);
                    }
                }
            },
            /*tk!(op @ T::Const) | tk!(op @ T::Let) => {
                let init_ = self.start();
                try!(lex());
                let kind = if op == T::Const {
                    VariableDeclarationKind::Const
                } else {
                    VariableDeclarationKind::Let
                };

                self.state.allow_in = false;
                let declarations = try!(self.parse_binding_list());
                // NOTE: Should we make sure this happens even if the above fails?
                self.state.allow_in = previous_allow_in;

                match self.lookahead {
                    tk!(T::In) if declarations.len() == 1 => {
                        let left = finish(&init_, self, VariableDeclaration {
                            declarations: declarations,
                            kind: kind,
                        });
                        try!(self.lex());
                        let right = try!(self.parse_expression());
                        inexp = Some((InitFor::Var(left), right));
                    },
                    _ => {
                        try!(self.consume_semicolon());
                        init = Some(InitFor::Var(finish(&init_, self, VariableDeclaration {
                            declarations: declarations,
                            kind: kind,
                        })));
                    }
                }
            },*/
            _ => {
                self.state.allow_in = false;
                let init_ = try!(self.parse_expression());
                self.state.allow_in = previous_allow_in;

                match self.lookahead {
                    tk!(T::In) => match init_ {
                        A(_, E::Identifier(_)) | A(_, E::Member(_)) => {
                            // NOTE: Should we make sure this happens even if the above fails?
                            try!(self.lex());
                            let left = init_;
                            let right = try!(self.parse_expression());
                            inexp = Some((InitFor::Exp(left), right));
                        },
                        _ => {
                            // tolerate
                            return Err(Error::InvalidLHSInForIn)
                        }
                    },
                    _ => {
                        expect!(self, T::Semi);
                        init = Some(InitFor::Exp(init_));
                        // NOTE: Should we make sure this happens even if the above fails?
                    }
                }
            }
        }

        if let None = inexp {
            if !tmatch!(self.lookahead, T::Semi) {
                test = Some(try!(self.parse_expression()));
            }
            expect!(self, T::Semi);

            if !tmatch!(self.lookahead, T::RParen) {
                update = Some(try!(self.parse_expression()));
            }
        }

        expect!(self, T::RParen);

        let old_in_iteration = self.state.in_iteration;
        self.state.in_iteration = true;

        let body = try!(self.parse_statement());

        // NOTE: Should we make sure this happens even if the above fails?
        self.state.in_iteration = old_in_iteration;

        Ok(finish(&node, self, match inexp {
            Some((InitFor::Var(left), right)) => Statement::ForIn(left, right, Box::new(body)),
            Some((InitFor::Exp(left), right)) => Statement::ForExpIn(left, right, Box::new(body)),
            None => match init {
                Some(InitFor::Var(init)) => Statement::For(init, test, update, Box::new(body)),
                Some(InitFor::Exp(init)) =>
                    Statement::ForExp(Some(init), test, update, Box::new(body)),
                None => Statement::ForExp(None, test, update, Box::new(body))
            }
        }))
    }

    // 12.7 The continue statement

    fn parse_continue_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Continue);

        // catch the very common case first: immediately a semicolon

        if self.has_line_terminator {
            if !self.state.in_iteration {
                return Err(Error::IllegalContinue);
            }

            return Ok(finish(&node, self, Statement::Continue(None)))
        }

        let label = match self.lookahead {
            Some(tok @ Token { ty: T::Identifier(_), .. }) => {
                let label = try!(self.parse_variable_identifier());

                // Guess: the vector is small enough that scanning through it is probably faster than
                // maintaining the hash.  If wrong, we can do it differently.
                if !self.state.label_set.iter().any( |l| *l == *label) {
                    return Err(Error::UnknownLabel(tok));
                }
                Some(label)
            },
            _ => None
        };

        try!(self.consume_semicolon());

        match label {
            None if !self.state.in_iteration => Err(Error::IllegalContinue),
            _ => Ok(finish(&node, self, Statement::Continue(label)))
        }
    }


    // 12.8 The break statement

    fn parse_break_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Break);

        // catch the very common case first: immediately a semicolon

        if self.has_line_terminator {
            if !(self.state.in_iteration || self.state.in_switch) {
                return Err(Error::IllegalBreak);
            }

            return Ok(finish(&node, self, Statement::Break(None)))
        }

        let label = match self.lookahead {
            Some(tok @ Token { ty: T::Identifier(_), .. }) => {
                let label = try!(self.parse_variable_identifier());

                // Guess: the vector is small enough that scanning through it is probably faster than
                // maintaining the hash.  If wrong, we can do it differently.
                if !self.state.label_set.iter().any( |l| *l == *label) {
                    return Err(Error::UnknownLabel(tok));
                }
                Some(label)
            },
            _ => None
        };

        try!(self.consume_semicolon());

        match label {
            None if !(self.state.in_iteration || self.state.in_switch) => Err(Error::IllegalBreak),
            _ => Ok(finish(&node, self, Statement::Break(label)))
        }
    }

    // 12.9 The return statement

    fn parse_return_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        let mut argument = None;

        expect!(self, T::Return);

        if !self.state.in_function_body {
            // tolerate
            return Err(Error::IllegalReturn)
        }

        // 'return' followed by a space and an identifier is very common...

        if self.has_line_terminator {
            // HACK
            return Ok(finish(&node, self, Statement::Return(None)));
        }

        if !tmatch!(self.lookahead, T::Semi, T::RBrack) && self.lookahead.is_some() {
            argument = Some(try!(self.parse_expression()));
        }

        try!(self.consume_semicolon());

        return Ok(finish(&node, self, Statement::Return(argument)));
    }

    // 12.10 The switch statement

    fn parse_switch_case(&mut self) -> PRes<'a, SCN<'a, Ann>> {
        let mut consequent = Vec::new();
        let node = self.start();

        let test = match self.lookahead {
            tk!(T::Default) => {
                try!(self.lex());
                SwitchCaseTest::Default
            },
            _ => {
                expect!(self, T::Case);
                SwitchCaseTest::Case(try!(self.parse_expression()))
            }
        };
        expect!(self, T::Colon);

        while let Some(tok) = self.lookahead {
            match tok.ty {
                T::RBrack | T::Default | T::Case => break,
                _ => {
                    let statement = try!(self.parse_statement_list_item());
                    consequent.push(statement);
                }
            }
        }

        Ok(finish(&node, self, SwitchCase { test: test, consequent: consequent }))
    }

    fn parse_switch_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Switch);

        expect!(self, T::LParen);

        let discriminant = try!(self.parse_expression());

        expect!(self, T::RParen);

        expect!(self, T::LBrack);

        let mut cases = Vec::new();

        if let tk!(T::RBrack) = self.lookahead {
            try!(self.lex());
            return Ok(finish(&node, self, Statement::Switch(discriminant, cases)))
        }

        let old_in_switch = self.state.in_switch;
        self.state.in_switch = true;
        let mut default_found = false;

        while let Some(tok) = self.lookahead {
            if let T::RBrack = tok.ty { break }
            let clause = try!(self.parse_switch_case());
            if let SwitchCaseTest::Default = clause.test {
                if default_found { return Err(Error::MultipleDefaultsInSwitch) }
                default_found = true;
            }
            cases.push(clause);
        }

        // NOTE: Should we make sure this is done even if the above fails?
        self.state.in_switch = old_in_switch;

        expect!(self, T::RBrack);

        Ok(finish(&node, self, Statement::Switch(discriminant, cases)))
    }

    // 12.13 The throw statement

    fn parse_throw_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Throw);

        if self.has_line_terminator { return Err(Error::NewlineAfterThrow) }

        let argument = try!(self.parse_expression());

        try!(self.consume_semicolon());

        Ok(finish(&node, self, Statement::Throw(argument)))
    }

    // 12.14 The try statement

    fn parse_catch_clause(&mut self) -> PRes<'a, CCN<'a, Ann>> {
        let node = self.start();

        expect!(self, T::Catch);

        expect!(self, T::LParen);
        if let Some(tok @ Token { ty: T::RParen, .. }) = self.lookahead {
            return Err(Error::UnexpectedToken(tok))
        }

        let param = try!(self.parse_variable_identifier());
        // 12.14.1
        if self.strict && is_restricted_word(&param) {
            // tolerate
            return Err(Error::StrictCatchVariable)
        }

        expect!(self, T::RParen);
        let start = self.start();
        let body = try!(self.parse_block());
        let body = finish(&start, self, body);

        Ok(finish(&node, self, CatchClause { param: param, body: body }))
    }

    fn parse_try_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Try);

        let start = self.start();
        let block = try!(self.parse_block());
        let block = finish(&start, self, block);

        let handler = match self.lookahead {
            tk!(T::Catch) => Some(try!(self.parse_catch_clause())),
            _ => None
        };

        let finalizer = match self.lookahead {
            tk!(T::Finally) => {
                try!(self.lex());
                let start = self.start();
                let finalizer = try!(self.parse_block());

                Some(finish(&start, self, finalizer))
            },
            _ => None
        };

        match (handler, finalizer) {
            (None, None) => Err(Error::NoCatchOrFinally),
            (handler, finalizer) =>
                Ok(finish(&node, self, Statement::Try(block, handler, finalizer)))
        }
    }

    // 12.15 The debugger statement

    fn parse_debugger_statement(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>> {
        expect!(self, T::Debugger);

        try!(self.consume_semicolon());

        Ok(finish(&node, self, Statement::Debugger))
    }

    fn parse_statement(&mut self) -> PRes<'a, SN<'a, Ann>> {
        let node = self.start();
        /*let stmt = */match self.lookahead {
            tk!(T::LBrack) => {
                let body = try!(self.parse_block());
                Ok(finish(&node, self, Statement::Block(body)))
            },
            tk!(T::Semi) => self.parse_empty_statement(node),
            //tk!(T::LParen) => return self.parse_expression_statement(node),
            tk!(T::Break) => self.parse_break_statement(node),
            tk!(T::Continue) => self.parse_continue_statement(node),
            tk!(T::Debugger) => self.parse_debugger_statement(node),
            tk!(T::Do) => self.parse_do_while_statement(node),
            tk!(T::For) => self.parse_for_statement(node),
            //tk!(T::Function) => self.parse_function_statement(node),
            tk!(T::If) => self.parse_if_statement(node),
            tk!(T::Return) => self.parse_return_statement(node),
            tk!(T::Switch) => self.parse_switch_statement(node),
            tk!(T::Throw) => self.parse_throw_statement(node),
            tk!(T::Try) => self.parse_try_statement(node),
            tk!(T::Var) => self.parse_variable_statement(node),
            tk!(T::While) => self.parse_while_statement(node),
            //tk!(T::With) => self.parse_with_statement(node),
            //Some(_) => {
            _ => {
                let expr = try!(self.parse_expression());

                // 12.12 Labelled Statements
                match (expr, self.lookahead) {
                    (A(ann, E::Identifier(name)), Some(tok @ Token { ty: T::Colon, .. })) => {
                        try!(self.lex());

                        if self.state.label_set.iter().any( |l| *l == name) {
                            return Err(Error::LabelRedeclaration(tok));
                        }

                        self.state.label_set.push(name);
                        let labeled_body = self.parse_statement();
                        // Make sure not to exit this function before removing state from the label
                        // set!
                        self.state.label_set.pop();
                        let name = A(ann, E::Identifier(name));
                        Ok(finish(&node, self,
                                  Statement::Labeled(name, Box::new(try!(labeled_body)))))
                    },
                    (expr, _) => {
                        try!(self.consume_semicolon());

                        Ok(finish(&node, self, Statement::Expression(expr)))
                    }
                }
            },
            //None => Err(Error::UnexpectedEOF),
        }/*;
        println!("Parsed statement: {:?}", stmt);
        stmt*/
    }

    #[inline]
    fn parse_function_source_elements_to_end(&mut self, body: &mut Block<'a, Ann>) -> PRes<'a, ()> {
        while let Some(token) = self.lookahead {
            if let T::RBrack = token.ty { break }
            let statement = try!(self.parse_statement_list_item());
            body.push(statement);
        }

        expect!(self, T::RBrack);

        Ok(())
    }

    fn parse_function_source_elements(&mut self) -> PRes<'a, BN<'a, Ann>> {
        let mut body = Vec::new();
        let mut first_restricted = None;
        let node = self.start();

        expect!(self, T::LBrack);

        while let Some(token) = self.lookahead {
            if !tmatch!(self.lookahead, T::StringLiteral(_), T::EscapedStringLiteral(_), T::OctalStringLiteral(_)) {
                break;
            }
            let statement = try!(self.parse_statement_list_item());
            match statement {
                A(_, SLI::Statement(A(_, Statement::Expression(A(_, E::String(_)))))) |
                A(_, SLI::Statement(A(_, Statement::Expression(A(_, E::EscapedString(_)))))) => {
                    match token.ty {
                        T::StringLiteral("use strict") => {
                            self.strict = true;
                            // tolerate
                            if let Some(e) = first_restricted { return Err(e) }
                        },
                        T::OctalStringLiteral(_) if first_restricted.is_none() => {
                            first_restricted = Some(Error::StrictOctalLiteral(token));
                        },
                        _ => body.push(statement)
                    }
                },
                _ => {
                    // this is not a directive
                    body.push(statement);
                    break
                }
            }
        }

        let allow_in = self.state.allow_in;
        let last_comment_start = self.state.last_comment_start;

        let old_state = mem::replace(&mut self.state, State {
            allow_in: allow_in,
            label_set: Vec::new(),
            parenthesis_count: 0,
            in_function_body: true,
            in_iteration: false,
            in_switch: false,
            last_comment_start: last_comment_start,
        });

        let res = self.parse_function_source_elements_to_end(&mut body);

        self.state = State {
            allow_in: self.state.allow_in,
            last_comment_start: self.state.last_comment_start,

            .. old_state
        };

        // We don't try! res until this point, to make sure the old state is restored (this may or
        // may not be a good thing...).
        try!(res);

        return Ok(finish(&node, self, body));
    }

    fn validate_param(&self,
                      params: &mut Params<'a, Ann>,
                      param: Token<'a>,
                      name: Identifier<'a>) {
        // Guess: the vector is small enough that scanning through it is probably faster than
        // maintaining the hash.  If wrong, we can do it differently.
        if self.strict {
            if is_restricted_word(name) {
                params.stricted = Some(Error::StrictParamName(param))
            }
            if params.params.iter().any( |p| **p == name ) {
                params.stricted = Some(Error::StrictParamDupe(param))
            }
        } else if let None = params.first_restricted {
            if is_restricted_word(name) {
                params.first_restricted = Some(Error::StrictParamName(param));
            } else if is_strict_mode_reserved_word(name) {
                params.first_restricted = Some(Error::StrictReservedWord(param));
            } else if params.params.iter().any( |p| **p == name ) {
                params.first_restricted = Some(Error::StrictParamDupe(param));
            }
        }
    }

    fn parse_param(&mut self, params: &mut Params<'a, Ann>) -> PRes<'a, bool> {
        let rest = match self.lookahead {
            tk!(T::Ellipsis) => {
                try!(self.lex());
                if tmatch!(self.lookahead, T::LBrack) { return Err(Error::ObjectPatternAsRestParameter) }
                true
            },
            _ => false
        };
        let token = match self.lookahead {
            Some(token) => token,
            None => return Err(Error::UnexpectedEOF),
        };

        let node = self.start();
        let param = try!(self.parse_variable_identifier());
        self.validate_param(params, token, &param);

        if let tk!(T::Eq) = self.lookahead {
            if rest { return Err(Error::DefaultRestParameter) }

            try!(self.lex());
            let def = try!(self.parse_assignment_expression());
            params.defaults.push(finish(&node, self, BindingElement::SingleName(param, def)));
        } else if rest {
            if !tmatch!(self.lookahead, T::RParen) {
                return Err(Error::ParameterAfterRestParameter);
            }
            params.rest = Some(param);
            return Ok(false)
        } else if params.defaults.is_empty() {
            params.params.push(param);
        } else {
            // Uninitialized default
            params.defaults.push(finish(&node, self, BindingElement::Uninitialized(param)));
        }
        Ok(!tmatch!(self.lookahead, T::RParen))
    }

    fn parse_params(&mut self, first_restricted: Option<Error<'a>>) -> PRes<'a, Params<'a, Ann>> {
        let mut params = Params {
            params: Vec::new(),
            defaults: Vec::new(),
            rest: None,
            stricted: None,
            first_restricted: first_restricted,
        };

        expect!(self, T::LParen);

        if !tmatch!(self.lookahead, T::RParen) {
            while try!(self.parse_param(&mut params)) {
                expect!(self, T::Comma);
            }
        }

        expect!(self, T::RParen);

        Ok(params)
    }

    fn parse_function_declaration(&mut self,
                                  node: <Ann as Annotation>::Start
                                 ) -> PRes<'a, SLIN<'a, Ann>> {
        expect!(self, T::Function);
        let id;
        let first_restricted;
        match self.lookahead {
            Some(token) => {
                id = try!(self.parse_variable_identifier());
                first_restricted = if self.strict {
                    if is_restricted_word(&id) {
                        // tolerate...
                        return Err(Error::StrictFunctionName(token))
                    } else { None }
                } else {
                     if is_restricted_word(&id) {
                         Some(Error::StrictFunctionName(token))
                     } else if is_strict_mode_reserved_word(&id) {
                         Some(Error::StrictReservedWord(token))
                     } else { None }
                };
            },
            None => return Err(Error::UnexpectedEOF)
        }

        let Params { params, defaults, stricted, first_restricted, rest } =
            try!(self.parse_params(first_restricted));

        let previous_strict = self.strict;
        let body = try!(self.parse_function_source_elements());
        if self.strict {
            if let Some(e) = first_restricted { return Err(e) }
            // tolerate
            if let Some(e) = stricted { return Err(e) }
        }
        self.strict = previous_strict;

        Ok(finish(&node, self, SLI::Function(id, F {
            params: params,
            defaults: defaults,
            rest: rest,
            body: body,
        })))
    }

    fn parse_function_expression(&mut self,
                                 node: <Ann as Annotation>::Start
                                ) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        expect!(self, T::Function);
        let first_restricted;
        let id = match self.lookahead {
            tk!(T::LParen) => {
                first_restricted = None;
                None
            },
            Some(token) => {
                let id = try!(self.parse_variable_identifier());
                first_restricted = if self.strict {
                    if is_restricted_word(&id) {
                        // tolerate...
                        return Err(Error::StrictFunctionName(token))
                    } else { None }
                } else {
                     if is_restricted_word(&id) {
                         Some(Error::StrictFunctionName(token))
                     } else if is_strict_mode_reserved_word(&id) {
                         Some(Error::StrictReservedWord(token))
                     } else { None }
                };
                Some(id)
            },
            None => return Err(Error::UnexpectedEOF)
        };

        let Params { params, defaults, stricted, first_restricted, rest } =
            try!(self.parse_params(first_restricted));

        let previous_strict = self.strict;
        let body = try!(self.parse_function_source_elements());
        if self.strict {
            if let Some(e) = first_restricted { return Err(e) }
            // tolerate
            if let Some(e) = stricted { return Err(e) }
        }
        self.strict = previous_strict;

        Ok(finish(&node, self, E::Function(id, F {
            params: params,
            defaults: defaults,
            rest: rest,
            body: body,
        })))
    }

    fn parse_statement_list_item(&mut self) -> PRes<'a, SLIN<'a, Ann>> {
        let node = self.start();

        match self.lookahead {
            tk!(T::Function) => self.parse_function_declaration(node),
            _ => self.parse_statement().map( |stmt| finish(&node, self, SLI::Statement(stmt)) )
        }
    }

    fn parse_script_body(&mut self) -> PRes<'a, Vec<SLIN<'a, Ann>>> {
        let mut body = Vec::new();
        let mut first_restricted = None;

        while let Some(token) = self.lookahead {
            let statement = try!(self.parse_statement_list_item());
            match statement {
                A(_, SLI::Statement(A(_, Statement::Expression(A(_, E::String(_)))))) |
                A(_, SLI::Statement(A(_, Statement::Expression(A(_, E::EscapedString(_)))))) => {
                    match token.ty {
                        T::StringLiteral("use strict") => {
                            self.strict = true;
                            // tolerate
                            if let Some(e) = first_restricted { return Err(e) }
                        },
                        T::OctalStringLiteral(_) if first_restricted.is_none() => {
                            first_restricted = Some(Error::StrictOctalLiteral(token));
                        },
                        _ => {}
                    }
                },
                _ => {
                    // this is not a directive
                    body.push(statement);
                    break
                }
            }
            body.push(statement);
        }

        while let Some(_) = self.lookahead {
            body.push(try!(self.parse_statement_list_item()));
        }

        Ok(body)
    }

    fn parse_program(&mut self) -> PRes<'a, ScriptN<'a, Ann>> {
        try!(self.peek());
        let node = self.start();
        self.strict = false;

        let body = try!(self.parse_script_body());
        Ok(finish(&node, self, body))
    }
}


pub fn parse<'a, Ann, Start>(root: &'a RootCtx, code: &'a str, /*options*/_: &Options) -> PRes<'a, ScriptN<'a, Ann>>
        where Ann: Annotation<Ctx=Ctx<'a, Ann, Start>, Start=Start>,
              //Ann: fmt::Debug,
    {
    let source = code;
    let index = 0;
    let line_number = if code.len() > 0 { 1 } else { 0 };
    let line_start = 0;

    let start_index = index;
    let start_line_number = line_number;
    let start_line_start = line_start;
    //let length = source.len();
    let lookahead = None;

    let state = State {
        allow_in: true,
        label_set: Vec::new(),
        parenthesis_count: 0,
        in_function_body: false,
        in_iteration: false,
        in_switch: false,
        last_comment_start: None,
    };

    let mut ctx = Ctx {
        source: source,
        index: index,
        line_number: line_number,
        line_start: line_start,

        start_index: start_index,
        start_line_number: start_line_number,
        start_line_start: start_line_start,
        //length: length,
        lookahead: lookahead,
        state: state,

        scanning: false,
        has_line_terminator: false,
        last_index: None,
        last_line_number: None,
        last_line_start: None,
        strict: false,

        rest: source,
        byte_index: 0,
        start_byte_index: 0,
        root: root,
        binop_stack: Vec::new(),
    };

    ctx.parse_program()
}

#[test]
fn it_works() {
    let root = RootCtx::new();
    println!("{:?}", parse::<(), ()>(&root, include_str!("../tests/test.js"), &Options).unwrap());
}
