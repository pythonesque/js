#![feature(core,collections,rustc_private,unicode)]

extern crate arena;
extern crate unicode;

use arena::TypedArena;

use ast::{Annotation, BindingElement, Block, finish, Identifier, Statement};
use ast::Annotated as A;
use ast::BlockNode as BN;
use ast::BindingElementNode as BEN;
use ast::Expression as E;
use ast::ExpressionNode as EN;
use ast::IdentifierNode as IN;
use ast::Function as F;
use ast::ScriptNode as ScriptN;
use ast::StatementListItem as SLI;
use ast::StatementListItemNode as SLIN;
use ast::StatementNode as SN;
use ast::Tok as T;

use std::char;
use std::collections::HashSet;
use std::error::FromError;
use std::fmt;
use std::mem;
use std::num::{self, ParseFloatError};

use unicode::str::Utf16Encoder;

mod ast;

pub type Pos = usize;

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
pub struct State<'a> {
    allow_in: bool,
    label_set: HashSet<&'a str>,
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

pub struct Ctx<'a> {
    source: &'a str,
    index: Pos,
    rest: &'a str,
    line_number: Pos,
    line_start: Pos,

    start_index: Pos,
    start_line_number: Pos,
    start_line_start: Pos,
    //length: usize,
    lookahead: Option<Token<'a>>,
    state: State<'a>,

    scanning: bool,
    has_line_terminator: bool,
    last_index: Option<Pos>,
    last_line_number: Option<Pos>,
    last_line_start: Option<Pos>,
    strict: bool,

    root: &'a RootCtx,
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
        self.ty.fmt(f)
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
    ExpectedIdent,
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
}

impl<'a> FromError<ParseFloatError> for Error<'a> {
    fn from_error(err: ParseFloatError) -> Self {
        Error::ParseFloat(err)
    }
}

pub type PRes<'a, T> = Result<T, Error<'a>>;

impl<'a> RootCtx {
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

enum ScanHex { U, X }

impl<'a> Annotation for () {
    type Ctx = Ctx<'a>;
    type Start = ();

    fn start(_: &Ctx) -> () {
        ()
    }

    fn finish(_: (), _: &Ctx) -> () {
        ()
    }
}

impl<'a> Ctx<'a> {
    #[cold] #[inline(never)]
    pub fn unexpected_char(&self, opt: Option<(char, &str)>) -> Error<'a> {
        match opt {
            Some((ch, _)) => Error::UnexpectedChar(ch),
            None => Error::UnexpectedEOF,
        }
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
            self.rest = rest;

            match ch {
                '\\' => match self.rest.slice_shift_char() {
                    Some(('u', rest)) => {
                        self.index += 1;
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
        let start_index = self.index;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            match ch {
                '\\' => {
                    // Blackslash (U+005C) marks Unicode escape sequence.
                    let mut id = self.root.ident_arena
                        .alloc(self.source[start_index..self.index].to_string());
                    self.index += 1;
                    self.rest = rest;
                    return self.get_escaped_identifier(&mut id).and(Ok(id));
                },
                ch if is_identifier_part(ch as u32) => {
                    self.index += 1;
                    self.rest = rest;
                },
                _ => break
            }
        }
        if self.rest.len() == 0 { return Err(Error::ExpectedIdent) }
        Ok(&self.source[start_index..self.index])
    }

    fn scan_identifier(&mut self) -> PRes<'a, Token<'a>> {
        let start = self.index;
        let id = try!(match self.rest.slice_shift_char() {
            Some(('\\', rest)) => {
                self.index += 1;
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
                                    rest = rest_;
                                    GtGtGtEq
                                },
                                _ => {
                                    self.index += 2;
                                    rest = rest_;
                                    GtGtGt
                                }
                            },
                            Some(('=', rest_)) => {
                                self.index += 2;
                                rest = rest_;
                                GtGtEq
                            },
                            _ => {
                                self.index += 1;
                                rest = rest_;
                                GtGt
                            }
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            GtEq
                        },
                        _ => Gt
                    },
                    '=' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => match rest_.slice_shift_char() {
                            Some(('=', rest_)) => {
                                self.index += 2;
                                rest = rest_;
                                EqEqEq
                            },
                            _ => {
                                self.index += 1;
                                rest = rest_;
                                EqEq
                            }
                        },
                        Some(('>', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            Arrow
                        },
                        _ => Eq
                    },
                    '!' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => match rest_.slice_shift_char() {
                            Some(('=', rest_)) => {
                                self.index += 2;
                                rest = rest_;
                                NEqEq
                            },
                            _ => {
                                self.index += 1;
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
                                rest = rest_;
                                LtLtEq
                            },
                            _ => {
                                self.index += 1;
                                rest = rest_;
                                LtLt
                            }
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            LtEq
                        },
                        _ => Lt
                    },
                    '&' => match rest.slice_shift_char() {
                        Some(('&', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            AndAnd
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            AndEq
                        },
                        _ => And
                    },
                    '|' => match rest.slice_shift_char() {
                        Some(('|', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            OrOr
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            OrEq
                        },
                        _ => Or
                    },
                    '+' => match rest.slice_shift_char() {
                        Some(('+', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            PlusPlus
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            PlusEq
                        },
                        _ => Plus
                    },
                    '-' => match rest.slice_shift_char() {
                        Some(('-', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            MinusMinus
                        },
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            MinusEq
                        },
                        _ => Minus
                    },
                    '*' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            TimesEq
                        },
                        _ => Times
                    },
                    '/' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            DivEq
                        },
                        _ => Div
                    },
                    '^' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            XorEq
                        },
                        _ => Xor
                    },
                    '%' => match rest.slice_shift_char() {
                        Some(('=', rest_)) => {
                            self.index += 1;
                            rest = rest_;
                            ModEq
                        },
                        _ => Mod
                    },
                    ch => return Err(Error::UnexpectedChar(ch))
                };
                self.index += 1;
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
        let begin = self.index;
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if let None = hex_digit(ch) { break }
            self.index += 1;
            self.rest = rest;
        }
        if begin == self.index { return Err(self.unexpected_char(self.rest.slice_shift_char())) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch as u32) { return Err(Error::UnexpectedChar(ch)) }
        }
        Ok(Token {
            ty: T::NumericLiteral(try!(num::from_str_radix(&self.source[begin..self.index], 16))),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_binary_literal(&mut self, start: Pos) -> PRes<'a, Token<'a>> {
        let begin = self.index;
        while let Some(('0'...'1', rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.rest = rest;
        }
        // only 0b or 0B
        if begin == self.index { return Err(self.unexpected_char(self.rest.slice_shift_char())) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch as u32) || is_decimal_digit(ch) { return Err(Error::UnexpectedChar(ch)) }
        }
        Ok(Token {
            ty: T::NumericLiteral(try!(num::from_str_radix(&self.source[begin..self.index], 2))),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_octal_literal(&mut self, prefix: char, rest: &'a str, start: Pos) -> PRes<'a, Token<'a>> {
        let octal = match octal_digit(prefix) { Some(_) => true, _ => false };
        let begin = if octal { self.index } else { self.index + 1 };

        self.index += 1;
        self.rest = rest;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if let None = octal_digit(ch) { break }
            self.index += 1;
            self.rest = rest;
        }
        // only 0o or 0O
        if begin == self.index { return Err(self.unexpected_char(self.rest.slice_shift_char())) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch as u32) || is_decimal_digit(ch) { return Err(Error::UnexpectedChar(ch)) }
        }
        Ok(Token {
            ty: T::OctalIntegerLiteral(try!(num::from_str_radix(&self.source[begin..self.index], 8))),
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

        if ch != '.' {
            self.index += 1;
            self.rest = rest;

            if ch == '0' {
                match self.rest.slice_shift_char() {
                    Some(('x', rest)) | Some(('X', rest)) => {
                        self.index += 1;
                        self.rest = rest;
                        return self.scan_hex_literal(start);
                    },
                    Some(('b', rest)) | Some(('B', rest)) => {
                        self.index += 1;
                        self.rest = rest;
                        return self.scan_binary_literal(start);
                    },
                    Some((ch @ 'o', rest)) | Some((ch @ 'O', rest)) => {
                        return self.scan_octal_literal(ch, rest, start);
                    },
                    Some((ch, rest)) => if let Some(_) = octal_digit(ch) {
                        if self.is_implicit_octal_literal() {
                            return self.scan_octal_literal(ch, rest, start);
                        }
                    },
                    _ => {}
                }
            }

            while let Some((ch, rest)) = self.rest.slice_shift_char() {
                if !is_decimal_digit(ch) { break }
                self.index += 1;
                self.rest = rest;
            }
        }

        if let Some(('.', rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.rest = rest;

            while let Some((ch, rest)) = self.rest.slice_shift_char() {
                if !is_decimal_digit(ch) { break }
                self.index += 1;
                self.rest = rest;
            }
        }

        match self.rest.slice_shift_char() {
            Some(('e', rest)) | Some(('E', rest)) => {
                self.index += 1;
                self.rest = rest;
                match self.rest.slice_shift_char() {
                    Some(('+', rest)) | Some(('-', rest)) => {
                        self.index += 1;
                        self.rest = rest;
                    },
                    _ => {}
                }
                if let Some((ch, rest)) = self.rest.slice_shift_char() {
                    if is_decimal_digit(ch) {
                        self.index += 1;
                        self.rest = rest;
                        while let Some((ch, rest)) = self.rest.slice_shift_char() {
                            if !is_decimal_digit(ch) { break }
                            self.index += 1;
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
            ty: T::NumericLiteral(try!(self.source[start..self.index].parse())),
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
            self.rest = rest;

            if ch == quote { break }
            else if ch == '\\' {
                match self.rest.slice_shift_char() {
                    Some((ch, rest)) => {
                        self.index += 1;
                        self.rest = rest;
                        if !is_line_terminator(ch) {
                            match ch {
                                'u' | 'x' => {
                                    match self.rest.slice_shift_char() {
                                        Some(('{', rest)) => {
                                            self.index += 1;
                                            self.rest = rest;
                                            try!(self.scan_unicode_code_point_escape(s));
                                        },
                                        _ => {
                                            let restore = self.index;
                                            let prefix = if ch == 'u' { ScanHex::U } else { ScanHex::X };
                                            match self.scan_hex_escape(prefix) {
                                                Ok(c) => s.push(c),
                                                _ => {
                                                    self.index = restore;
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
                                                self.rest = rest;

                                                // 3 digits are only allowed when string starts
                                                // with 0, 1, 2, 3
                                                if let '0'...'3' = ch {
                                                    if let Some((ch_, rest)) = self.rest.slice_shift_char() {
                                                        if let Some(code_) = octal_digit(ch_) {
                                                            self.index += 1;
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

    fn scan_string_literal(&mut self, quote: char) -> PRes<'a, Token<'a>>
    {
        let start = self.index - 1;

        loop {
            match self.rest.slice_shift_char() {
                Some((ch, rest)) => {
                    if ch == quote {
                        self.index += 1;
                        self.rest = rest;
                        break
                    }
                    else if ch == '\\' {
                        let s = self.root.string_arena
                            .alloc(Utf16Encoder::new(self.source[start + 1..self.index].chars())
                                   .collect());
                        return self.scan_escaped_string_literal(quote, start, s);
                    } else {
                        self.index += 1;
                        self.rest = rest;
                        if is_line_terminator(ch) { return Err(Error::UnexpectedChar(ch)); }
                    }
                },
                None => { return Err(Error::UnexpectedEOF) }
            }
        }
        Ok(Token {
            ty: T::StringLiteral(&self.source[start + 1..self.index - 1]),
            line_number: self.start_line_number,
            line_start: self.start_line_start,
            start: start,
            end: self.index
        })
    }

    fn skip_single_line_comment(&mut self, /*offset*/_: usize) {
        //let start = self.index - offset;
        //let loc =
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.rest = rest;
            if is_line_terminator(ch) {
                self.has_line_terminator = true;
                // if extra.comments ...
                if ch == '\x0D' {
                    if let Some(('\x0A', rest)) = self.rest.slice_shift_char() {
                        self.index += 1;
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
                            self.rest = rest_;
                        },
                        _ => self.rest = rest
                    }
                } else { self.rest = rest }
                self.has_line_terminator = true;
                self.line_number += 1;
                self.index += 1;
                self.line_start = self.index;
            } else if ch == '*' {
                // Block comment ends with '*/'.
                if let Some(('/', rest)) = rest.slice_shift_char() {
                    self.index += 2;
                    self.rest = rest;
                    // if extra.comments ...
                    return Ok(());
                }
                self.index += 1;
                self.rest = rest;
            } else {
                self.index += 1;
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
                self.rest = rest;
            } else if is_line_terminator(ch) {
                self.has_line_terminator = true;
                self.index += 1;
                self.rest = rest;
                if ch == '\x0D' {
                    if let Some(('\x0A', rest)) = self.rest.slice_shift_char() {
                        self.rest = rest;
                        self.index += 1;
                    }
                }
                self.line_number += 1;
                self.line_start = self.index;
                start = true;
            } else if ch == '/' {
                match rest.slice_shift_char() {
                    Some(('/', rest)) => {
                        self.index += 2;
                        self.rest = rest;
                        self.skip_single_line_comment(2);
                        start = true;
                    },
                    Some(('*', rest)) => {
                        self.index += 2;
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
        self.start_line_number = self.line_number;
        self.start_line_start = self.line_start;

        self.lookahead = match try!(self.advance()) {
            Token { ty: T::EOF, .. } => None,
            tok => Some(/* extra.tokens*/tok)
        };
        println!("{:?}", self.lookahead);

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
        self.start_line_number = self.line_number;
        self.start_line_start = self.line_start;

        self.lookahead = match try!(self.advance()) {
            Token { ty: T::EOF, .. } => None,
            tok => Some(/* extra.tokens */tok)
        };
        println!("{:?}", self.lookahead);

        self.scanning = false;

        Ok(())
    }

}

struct Params<'a, Ann> {
    params: Vec<IN<'a, Ann>>,
    pub defaults: Vec<BEN<'a, Ann>>,
    rest: Option<IN<'a, Ann>>,
    stricted: Option<Error<'a>>,
    first_restricted: Option<Error<'a>>,
}

impl<'a> Ctx<'a> {
    #[cold] #[inline(never)]
    pub fn unexpected_token(&self, opt: Option<Token<'a>>) -> Error<'a> {
        match opt {
            Some(tok) => Error::UnexpectedToken(tok),
            None => Error::UnexpectedEOF,
        }
    }

    fn consume_semicolon(&mut self) -> PRes<'a, ()> {
        if Some(';') == self.rest.chars().next() { try!(self.lex()); return Ok(()) }
        if let Some(Token { ty: T::Semi, .. } ) = self.lookahead { try!(self.lex()); return Ok(()) }

        if self.has_line_terminator { return Ok(()) }

        self.last_index = Some(self.start_index);
        self.last_line_number = Some(self.start_line_number);
        self.last_line_start = Some(self.start_line_start);

        match self.lookahead {
            Some(Token { ty: T::RBrack, .. }) | None => Ok(()),
            o => Err(self.unexpected_token(o))
        }
    }

    fn start<Ann>(&self) -> <Ann as Annotation>::Start
        where Ann: Annotation<Ctx=Self>
    {
        <Ann as Annotation>::start(self)
    }

    fn parse_primary_expression<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();
        if let Some(token) = self.lookahead {
            let exp = match token.ty {
                // LParen =>
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
                // Div | DivEq => ... regexp ...
                T::BooleanLiteral(b) => { try!(self.lex()); E::Bool(b) },
                T::NullLiteral => { try!(self.lex()); E::Null },
                _ => {
                    try!(self.lex());
                    return Err(Error::UnexpectedToken(token))
                }
            };
            Ok(finish(node, self, exp))
        } else {
            return Err(Error::UnexpectedEOF)
        }
    }

    fn parse_left_hand_side_expression_allow_call<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_primary_expression();

        expr
    }

    fn parse_postfix_expression<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_left_hand_side_expression_allow_call();

        expr
    }

    fn parse_unary_expression<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_postfix_expression();

        expr
    }

    fn parse_binary_expression<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let left = self.parse_unary_expression();

        left
    }

    fn parse_conditional_expression<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_binary_expression();

        expr
    }

    fn parse_assignment_expression<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_conditional_expression();

        expr
    }

    fn parse_expression<Ann>(&mut self) -> PRes<'a, EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_assignment_expression();

        expr
    }

    fn parse_statement_list<Ann>(&mut self) -> PRes<'a, Block<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let mut list = Vec::new();
        while let Some(token) = self.lookahead {
            if let T::RBrack = token.ty { break }
            list.push(try!(self.parse_statement_list_item()));
        }

        Ok(list)
    }

    fn parse_block<Ann>(&mut self) -> PRes<'a, Block<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        expect!(self, T::LBrack);

        let block = try!(self.parse_statement_list());

        expect!(self, T::RBrack);

        Ok(block)
    }

    fn parse_variable_identifier<Ann>(&mut self) -> PRes<'a, IN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();
        let token = self.lookahead;
        try!(self.lex());
        match token {
            Some(Token { ty: T::Identifier(i), .. }) => Ok(finish(node, self, i)),
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

    fn parse_empty_statement<Ann>(&mut self, node: <Ann as Annotation>::Start) -> PRes<'a, SN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        expect!(self, T::Semi);
        Ok(finish(node, self, Statement::Empty))
    }

    fn parse_statement<Ann>(&mut self) -> PRes<'a, SN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();
        match self.lookahead {
            tk!(T::LBrack) => {
                let body = try!(self.parse_block());
                Ok(finish(node, self, Statement::Block(body)))
            },
            tk!(T::Semi) => self.parse_empty_statement(node),
            //tk!(T::LParen) => return self.parse_expression_statement(node),
            //Some(_) => {
            _ => {
                let expr = try!(self.parse_expression());

                try!(self.consume_semicolon());

                Ok(finish(node, self, Statement::Expression(expr)))
            },
            //None => Err(Error::UnexpectedEOF),
        }
    }

    fn parse_function_source_elements<Ann>(&mut self) -> PRes<'a, BN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let mut body = Vec::new();
        let mut first_restricted = None;
        let node = self.start::<Ann>();

        expect!(self, T::LBrack);

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
            label_set: HashSet::new(),
            parenthesis_count: 0,
            in_function_body: true,
            in_iteration: false,
            in_switch: false,
            last_comment_start: last_comment_start,
        });

        while let Some(token) = self.lookahead {
            if let T::RBrack = token.ty { break }
            let statement = try!(self.parse_statement_list_item());
            body.push(statement);
        }

        expect!(self, T::RBrack);

         self.state = State {
            allow_in: self.state.allow_in,
            last_comment_start: self.state.last_comment_start,

            .. old_state
        };

        return Ok(finish(node, self, body));
    }

    fn validate_param<Ann>(&self,
                           params: &mut Params<'a, Ann>,
                           param: Token<'a>,
                           name: Identifier<'a>)
        where Ann: Annotation<Ctx=Self>
    {
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

    fn parse_param<Ann>(&mut self, params: &mut Params<'a, Ann>) -> PRes<'a, bool>
        where Ann: Annotation<Ctx=Self>
    {
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

        let node = self.start::<Ann>();
        let param = try!(self.parse_variable_identifier());
        self.validate_param(params, token, &param);

        if let tk!(T::Eq) = self.lookahead {
            if rest { return Err(Error::DefaultRestParameter) }

            try!(self.lex());
            let def = try!(self.parse_assignment_expression());
            params.defaults.push(finish(node, self, BindingElement::SingleName(param, def)));
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
            params.defaults.push(finish(node, self, BindingElement::Uninitialized(param)));
        }
        Ok(!tmatch!(self.lookahead, T::RParen))
    }

    fn parse_params<Ann>(&mut self, first_restricted: Option<Error<'a>>) -> PRes<'a, Params<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
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

    fn parse_function_declaration<Ann>(&mut self,
                                       node: <Ann as Annotation>::Start
                                      ) -> PRes<'a, SLIN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
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

        Ok(finish(node, self, SLI::Function(id, F {
            params: params,
            defaults: defaults,
            rest: rest,
            body: body,
        })))
    }

    fn parse_function_expression<Ann>(&mut self,
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

        Ok(finish(node, self, E::Function(id, F {
            params: params,
            defaults: defaults,
            rest: rest,
            body: body,
        })))
    }

    fn parse_statement_list_item<Ann>(&mut self) -> PRes<'a, SLIN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();

        match self.lookahead {
            tk!(T::Function) => self.parse_function_declaration(node),
            _ => self.parse_statement().map( |stmt| finish(node, self, SLI::Statement(stmt)) )
        }
    }

    fn parse_script_body<Ann>(&mut self) -> PRes<'a, Vec<SLIN<'a, Ann>>>
        where Ann: Annotation<Ctx=Self>
    {
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

    fn parse_program<Ann>(&mut self) -> PRes<'a, ScriptN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        try!(self.peek());
        let node = self.start::<Ann>();
        self.strict = false;

        let body = try!(self.parse_script_body());
        Ok(finish(node, self, body))
    }
}


pub fn parse<'a, Ann>(root: &'a RootCtx, code: &'a str, /*options*/_: &Options) -> PRes<'a, ScriptN<'a, Ann>>
        where Ann: Annotation<Ctx=Ctx<'a>>
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
        label_set: HashSet::new(),
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

        root: root,
    };

    ctx.parse_program()
}

#[test]
fn it_works() {
    let root = RootCtx::new();
    println!("{:?}", parse::<()>(&root, include_str!("../tests/test.js"), &Options).unwrap());
}
