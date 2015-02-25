#![feature(box_syntax,core,collections,rustc_private)]

extern crate arena;

use arena::TypedArena;

use ast::{Annotation, finish, Statement};
use ast::Annotated as A;
use ast::BlockNode as BN;
use ast::Expression as E;
use ast::ExpressionNode as EN;
use ast::IdentifierNode as IN;
use ast::Function as F;
use ast::FunctionNode as FN;
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

mod ast;

pub type Pos = usize;

macro_rules! expect {
    ($e:expr, $pat:pat) => {
        if let Some(Token { ty: $pat , .. }) = $e.lookahead {
            try!($e.lex());
        } else {
            return Err(Error::UnexpectedToken)
        }
    }
}

macro_rules! tmatch {
    ($e:expr, $pat:pat) => {
        if let Some(Token { ty: $pat, .. }) = $e { true } else { false }
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
    arena: TypedArena<String>
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
pub enum Error {
    UnexpectedToken,
    ExpectedHex,
    InvalidUnicode,
    ExpectedIdent,
    UnterminatedStringLiteral,
    UnexpectedEOF,
    ParseFloat(ParseFloatError),
}

impl FromError<ParseFloatError> for Error {
    fn from_error(err: ParseFloatError) -> Self {
        Error::ParseFloat(err)
    }
}

pub type PRes<T> = Result<T, Error>;

impl<'a> RootCtx {
    pub fn new() -> Self {
        RootCtx {
            arena: TypedArena::new(),
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

fn is_identifier_start(ch: char) -> bool {
    match ch {
        '$' | '_' |
        'A'...'Z' |
        'a'...'z' |
        '\\'
        // | ch if non_ascii_regex_that_is_ridiculous
        => true,
        _ => false
    }
}

fn is_identifier_part(ch: char) -> bool {
    match ch {
        '$' | '_' |
        'A'...'Z' |
        'a'...'z' |
        '0'...'9' |
        '\\'
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

fn keyword<'a>(id: &str) -> Option<T<'a>> {
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
    fn scan_hex_escape(&mut self, prefix: ScanHex) -> PRes<char> {
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
                    code = code * 16 + i;
                },
                None => return Err(Error::ExpectedHex)
            }
            i += 1;
        }
        if self.rest.len() == 0 { return Err(Error::UnexpectedEOF) }
        match char::from_u32(code) {
            Some(ch) => Ok(ch),
            None => Err(Error::InvalidUnicode)
        }
    }

    fn scan_unicode_code_point_escape(&mut self) -> PRes<char> {
        if let Some('}') = self.rest.chars().next() { return Err(Error::UnexpectedToken) }
        let mut code = 0;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.rest = rest;
            match hex_digit(ch) {
                Some(code_) => code = code * 16 + code_,
                None if ch != '}' => return Err(Error::UnexpectedToken),
                None => break
            }
            if code > 0x10FFFF { return Err(Error::UnexpectedToken) }
        }
        //if (code <= 0xFFFF) {
            return match char::from_u32(code) {
                Some(ch) => Ok(ch),
                None => Err(Error::InvalidUnicode)
            }
        //}
        //let cu1 = ((code - 0x10000) >> 10) + 0xD800;
        //let cu2 = ((code - 0x10000) & 1023) + 0xDC00;
    }

    fn get_escaped_identifier(&mut self) -> PRes<&'a str> {
        // '\u' (U+005C, U+0075) denotes an escaped character.
        let mut id = match self.rest.slice_shift_char() {
            Some(('u', rest)) => {
                self.index += 1;
                self.rest = rest;
                match try!(self.scan_hex_escape(ScanHex::U)) {
                    '\\' => return Err(Error::UnexpectedToken),
                    ch if !is_identifier_start(ch) => return Err(Error::UnexpectedToken),
                    ch => ch
                }.to_string()
            },
            _ => return Err(Error::UnexpectedToken)
        };

        // '\u' (U+005C, U+0075) denotes an escaped character.
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if !is_identifier_part(ch) { break }
            self.index += 1;
            self.rest = rest;

            match ch {
                '\\' => match self.rest.slice_shift_char() {
                    Some(('u', rest)) => {
                        self.index += 1;
                        self.rest = rest;
                        id.push(match try!(self.scan_hex_escape(ScanHex::U)) {
                            '\\' => return Err(Error::UnexpectedToken),
                            ch if !is_identifier_part(ch) => return Err(Error::UnexpectedToken),
                            ch => ch
                        });
                    },
                    _ => return Err(Error::UnexpectedToken)
                },
                _ => id.push(ch)
            }
        }

        Ok(&**self.root.arena.alloc(id))
    }

    fn get_identifier(&mut self) -> PRes<&'a str> {
        let start_index = self.index;
        let start_rest = self.rest;

        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            match ch {
                '\\' => {
                    // Blackslash (U+005C) marks Unicode escape sequence.
                    self.index = start_index;
                    self.rest = start_rest;
                    return self.get_escaped_identifier();
                },
                ch if is_identifier_part(ch) => {
                    self.index += 1;
                    self.rest = rest;
                },
                _ => break
            }
        }
        if self.rest.len() == 0 { return Err(Error::ExpectedIdent) }
        Ok(&self.source[start_index..self.index])
    }

    fn scan_identifier(&mut self) -> PRes<Token<'a>> {
        let start = self.index;
        let id = try!(match self.rest.slice_shift_char() {
            Some(('\\', rest)) => {
                self.index += 1;
                self.rest = rest;
                self.get_escaped_identifier()
            },
            _ => self.get_identifier()
        });

        let ty = if id.len() == 1 {
            T::Identifier(id)
        } else if let Some(k) = keyword(id) {
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

    fn scan_punctuator(&mut self) -> PRes<Token<'a>> {
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
                    _ => return Err(Error::UnexpectedToken)
                };
                self.index += 1;
                self.rest = rest;
                ty
            },
            None => return Err(Error::UnexpectedToken)
        };
        Ok(Token {
            ty: ty,
            line_number: line_number,
            line_start: line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_hex_literal(&mut self, start: Pos) -> PRes<Token<'a>> {
        let begin = self.index;
        while let Some((ch, rest)) = self.rest.slice_shift_char() {
            if let None = hex_digit(ch) { break }
            self.index += 1;
            self.rest = rest;
        }
        if begin == self.index { return Err(Error::UnexpectedToken) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch) { return Err(Error::UnexpectedToken) }
        }
        Ok(Token {
            ty: T::NumericLiteral(try!(num::from_str_radix(&self.source[begin..self.index], 16))),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_binary_literal(&mut self, start: Pos) -> PRes<Token<'a>> {
        let begin = self.index;
        while let Some(('0'...'1', rest)) = self.rest.slice_shift_char() {
            self.index += 1;
            self.rest = rest;
        }
        // only 0b or 0B
        if begin == self.index { return Err(Error::UnexpectedToken) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch) || is_decimal_digit(ch) { return Err(Error::UnexpectedToken) }
        }
        Ok(Token {
            ty: T::NumericLiteral(try!(num::from_str_radix(&self.source[begin..self.index], 2))),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_octal_literal(&mut self, prefix: char, rest: &'a str, start: Pos) -> PRes<Token<'a>> {
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
        if begin == self.index { return Err(Error::UnexpectedToken) }
        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch) || is_decimal_digit(ch) { return Err(Error::UnexpectedToken) }
        }
        Ok(Token {
            ty: T::NumericLiteral(try!(num::from_str_radix(&self.source[begin..self.index], 8))),
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

    fn scan_numeric_literal(&mut self, ch: char, rest: &'a str) -> PRes<Token<'a>> {
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
                    } else { return Err(Error::UnexpectedToken); }
                }
            },
            _ => {}
        }

        if let Some(ch) = self.rest.chars().next() {
            if is_identifier_start(ch) { return Err(Error::UnexpectedToken) }
        }

        Ok(Token {
            ty: T::NumericLiteral(try!(self.source[start..self.index].parse())),
            line_number: self.line_number,
            line_start: self.line_start,
            start: start,
            end: self.index,
        })
    }

    fn scan_string_literal(&mut self, quote: char) -> PRes<Token<'a>> {
        let mut s = String::new();
        let mut octal = false;
        let start = self.index - 1;

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
                                            s.push(try!(self.scan_unicode_code_point_escape()));
                                        },
                                        _ => {
                                            let restore = self.index;
                                            let prefix = if ch == 'u' { ScanHex::U } else { ScanHex::X };
                                            match self.scan_hex_escape(prefix) {
                                                Ok(c) => s.push(c),
                                                //Err(Error::InvalidUnicode) => return Err(Error::InvalidUnicode),
                                                _ => {
                                                    self.index = restore;
                                                    self.rest = rest;
                                                    s.push(ch);
                                                }
                                            }
                                        }
                                    }
                                },
                                'n' => s.push('\n'),
                                'r' => s.push('\r'),
                                't' => s.push('\t'),
                                'b' => s.push('\x08'),
                                'f' => s.push('\x0C'),
                                'v' => s.push('\x0B'),
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
                                        match char::from_u32(code as u32) {
                                            Some(ch) => s.push(ch),
                                            None => return Err(Error::InvalidUnicode)
                                        }
                                    },
                                    None => s.push(ch)
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
                return Err(Error::UnexpectedToken);
            } else {
                s.push(ch);
            }
        }
        if self.rest.len() == 0 { return Err(Error::UnexpectedToken) }
        let s = &**self.root.arena.alloc(s);
        Ok(Token {
            ty: if octal { T::OctalLiteral(s) } else { T::StringLiteral(s) },
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

    fn skip_multi_line_comment(&mut self) -> PRes<()> {
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

        Err(Error::UnexpectedToken)
    }

    fn skip_comment(&mut self) -> PRes<()> {
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

    fn advance(&mut self) -> PRes<Token<'a>> {
        match self.rest.slice_shift_char() {
            None => Ok(Token {
                ty: T::EOF,
                line_number: self.line_number,
                line_start: self.line_start,
                start: self.index,
                end: self.index,
            }),
            Some((ch, _)) if is_identifier_start(ch) => self.scan_identifier(),
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

    fn lex(&mut self) -> PRes</*Token<'a>*/()> {
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

    fn peek(&mut self) -> PRes<()> {
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

    fn consume_semicolon(&mut self) -> PRes<()> {
        if Some(';') == self.rest.chars().next() { try!(self.lex()); return Ok(()) }
        if let Some(Token { ty: T::Semi, .. } ) = self.lookahead { try!(self.lex()); return Ok(()) }

        if self.has_line_terminator { return Ok(()) }

        self.last_index = Some(self.start_index);
        self.last_line_number = Some(self.start_line_number);
        self.last_line_start = Some(self.start_line_start);

        match self.lookahead {
            Some(Token { ty: T::RBrack, .. }) | None => Ok(()),
            _ => Err(Error::UnexpectedToken)
        }
    }
}

impl<'a> Ctx<'a> {
    fn start<Ann>(&self) -> <Ann as Annotation>::Start
        where Ann: Annotation<Ctx=Self>
    {
        <Ann as Annotation>::start(self)
    }

    fn parse_primary_expression<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();
        if let Some(Token { ty, .. }) = self.lookahead {
            let exp = match ty {
                T::Identifier(v) => { try!(self.lex()); E::Identifier(v) },
                T::StringLiteral(s) => { try!(self.lex()); E::String(s) },
                T::NumericLiteral(n) => { try!(self.lex()); E::Number(n) },
                T::OctalLiteral(o) => {
                    //if self.strict { tolerate_unexpected_token(lookahead, StrictOctal); }
                    try!(self.lex());
                    E::String(o)
                },
                T::Function => {
                    let (name, function) = try!(self.parse_function_expression());
                    E::Function(name, function)
                },
                _ => {
                    return Err(Error::UnexpectedToken)
                }
            };
            Ok(finish(node, self, exp))
        } else {
            return Err(Error::UnexpectedEOF)
        }
    }

    fn parse_left_hand_side_expression_allow_call<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_primary_expression();

        expr
    }

    fn parse_postfix_expression<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_left_hand_side_expression_allow_call();

        expr
    }

    fn parse_unary_expression<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_postfix_expression();

        expr
    }

    fn parse_binary_expression<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let left = self.parse_unary_expression();

        left
    }

    fn parse_conditional_expression<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_binary_expression();

        expr
    }

    fn parse_assignment_expression<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_conditional_expression();

        expr
    }

    fn parse_expression<Ann>(&mut self) -> PRes<EN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let expr = self.parse_assignment_expression();

        expr
    }

    fn parse_variable_identifier<Ann>(&mut self) -> PRes<IN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();
        let token = self.lookahead;
        try!(self.lex());
        match token {
            Some(Token { ty: T::Identifier(i), .. }) => Ok(finish(node, self, i)),
            _ => {
                // if self.strict && tmatch!(token, ) ...
                Err(Error::UnexpectedToken)
            }
        }
    }

    fn parse_empty_statement<Ann>(&mut self, node: <Ann as Annotation>::Start) -> PRes<SN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        expect!(self, T::Semi);
        Ok(finish(node, self, Statement::Empty))
    }

    fn parse_statement<Ann>(&mut self) -> PRes<SN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();
        match self.lookahead {
            Some(Token { ty, .. }) => match ty {
                //T::LBrack => return parse_block(),
                T::Semi => return self.parse_empty_statement(node),
                _ => {}
            },
            None => return Err(Error::UnexpectedToken),
        }

        let expr = try!(self.parse_expression());

        try!(self.consume_semicolon());

        Ok(finish(node, self, Statement::Expression(expr)))
    }

    fn parse_function_source_elements<Ann>(&mut self) -> PRes<BN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let mut body = Vec::new();
        let node = self.start::<Ann>();

        expect!(self, T::LBrack);

        while let Some(Token { ty: T::StringLiteral(_), .. }) = self.lookahead {
             let statement = try!(self.parse_statement_list_item());
             let first_restricted = false;
             if let A(_, SLI::Statement(A(_, Statement::Expression(A(_,E::String(directive)))))) = statement {
                match directive {
                    "use strict" => {
                        self.strict = true;
                        if first_restricted
                            { /*tolerate_unexpected_token(first_restricted, StrictOctalLiteral)*/ }
                    },
                    _ => {
                        /*if !first_restricted {
                            if let OctalLiteral(_) = token {
                                first_restricted = token;
                            }
                        }*/
                    }
                }
            } else {
                body.push(statement);
                break
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
            if let Token { ty: T::RBrack, .. } = token { break }
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

    fn parse_params<Ann>(&mut self, /*first_restricted*/_: Option<&'a str>) -> PRes<(Vec<IN<'a, Ann>>, /*defaults, */ Option<IN<'a, Ann>>)>
        where Ann: Annotation<Ctx=Self>
    {
        let params = Vec::new();
        expect!(self, T::LParen);

        if !tmatch!(self.lookahead, T::RParen) {
            expect!(self, T::Comma);
        }

        expect!(self, T::RParen);
        Ok((params, None))
    }

    fn parse_function_expression<Ann>(&mut self) -> PRes<(Option<IN<'a, Ann>>, FN<'a, Ann>)>
        where Ann: Annotation<Ctx=Self>
    {
        let mut id = None;
        let first_restricted = None;
        let node = self.start::<Ann>();

        expect!(self, T::Function);
        if !tmatch!(self.lookahead, T::LParen) {
            //let token = self.lookahead;
            id = Some(try!(self.parse_variable_identifier()));
            // if strict ...
                // if is_restricted_word { firstrestricted = token }
                // else if strictmode....
        }

        let (params, rest) = try!(self.parse_params(first_restricted));

        let body = try!(self.parse_function_source_elements());

        Ok((id, finish(node, self, F {
            params: params,
            rest: rest,
            body: body,
        })))
    }

    fn parse_statement_list_item<Ann>(&mut self) -> PRes<SLIN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        let node = self.start::<Ann>();
        self.parse_statement().map( |stmt| finish(node, self, SLI::Statement(stmt)) )
    }

    fn parse_script_body<Ann>(&mut self) -> PRes<Vec<SLIN<'a, Ann>>>
        where Ann: Annotation<Ctx=Self>
    {
        let mut body = Vec::new();
        let first_restricted = false;

        while let Some(Token { ty: T::StringLiteral(_), .. }) = self.lookahead {
             let statement = try!(self.parse_statement_list_item());
             if let A(_, SLI::Statement(A(_, Statement::Expression(A(_,E::String(directive)))))) = statement {
                match directive {
                    "use strict" => {
                        self.strict = true;
                        if first_restricted
                            { /*tolerate_unexpected_token(first_restricted, StrictOctalLiteral)*/ }
                    },
                    _ => {
                        /*if !first_restricted {
                            if let OctalLiteral(_) = token {
                                first_restricted = token;
                            }
                        }*/
                    }
                }
            } else {
                body.push(statement);
                break
            }
            body.push(statement);
        }

        while let Some(_) = self.lookahead {
            let statement = try!(self.parse_statement_list_item());
            body.push(statement);
        }

        Ok(body)
    }

    fn parse_program<Ann>(&mut self) -> PRes<ScriptN<'a, Ann>>
        where Ann: Annotation<Ctx=Self>
    {
        try!(self.peek());
        let node = self.start::<Ann>();
        self.strict = false;

        let body = try!(self.parse_script_body());
        Ok(finish(node, self, body))
    }
}


pub fn parse<'a, Ann>(root: &'a RootCtx, code: &'a str, /*options*/_: Options) -> PRes<ScriptN<'a, Ann>>
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
    println!("{:?}", parse::<()>(&root, include_str!("../tests/test.js"), Options).unwrap());
}
