#![feature(collections)]
#![feature(rustc_private)]

extern crate arena;

use arena::TypedArena;

use ast::{Program, Tok};
use ast::Tok::*;

use std::char;
use std::collections::HashSet;

mod ast;

type Pos = usize;

#[derive(Default)]
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
    length: usize,
    lookahead: Option<Token<'a>>,
    state: State<'a>,

    scanning: bool,
    has_line_terminator: bool,
    last_index: Option<Pos>,
    last_line_number: Option<Pos>,
    last_line_start: Option<Pos>,

    root: &'a RootCtx,
}

#[derive(Debug)]
pub struct Token<'a> {
    ty: Tok<'a>,
    line_number: Pos,
    line_start: Pos,
    start: Pos,
    end: Pos
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

fn is_keyword(id: &str) -> bool{
    // if strict ...
    match id {
        "if" | "in" | "do" |
        "var" | "for" | "new" | "try" | "let" |
        "this" | "else" | "case" | "void" | "with" | "enum" |
        "while" | "break" | "catch" | "throw" | "const" | "yield" | "class" | "super" |
        "return" | "typeof" | "delete" | "switch" | "export" | "import" |
        "default" | "finally" | "extends" |
        "function" | "continue" | "debugger" |
        "instanceof" => true,
        _ => false
    }
}

enum ScanHex { U, X }

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
        match char::from_u32(code) {
            Some(ch) => Ok(ch),
            None => Err(Error::InvalidUnicode)
        }
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
            Identifier(id)
        } else if is_keyword(id) {
            Keyword(id)
        } else if id == "null" {
            NullLiteral(id)
        } else if id == "true" || id == "false" {
            BooleanLiteral(id)
        } else {
            Identifier(id)
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

    fn skip_single_line_comment(&mut self, offset: usize) {
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
                ty: EOF,
                line_number: self.line_number,
                line_start: self.line_start,
                start: self.index,
                end: self.index,
            }),
            Some((ch, _)) if is_identifier_start(ch) => self.scan_identifier(),
            Some(('(', _)) | Some((')', _)) | Some((';', _)) => self.scan_punctuator(),
            Some(('\'', _)) | Some(('"', _)) => return Err(Error::UnexpectedToken),//self.scan_string_literal(),
            Some(('.', rest)) => match rest.slice_shift_char() {
                Some((ch, _)) if is_decimal_digit(ch) => return Err(Error::UnexpectedToken),//self.scan_numeric_literal(),
                _ => self.scan_punctuator()
            },
            Some((ch, _)) if is_decimal_digit(ch) => return Err(Error::UnexpectedToken),//self.scan_numeric_literal(),
            // if extra.tokenize ...
            _ => self.scan_punctuator()
        }
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

        self.lookahead = Some(/* extra.tokens */ try!(self.advance()));
        println!("{:?}", self.lookahead);

        self.scanning = false;

        Ok(())
    }

    fn parse_program(&mut self) -> PRes<Program> {
        try!(self.peek());
        Ok(Program)
    }
}


pub fn parse<'a>(root: &'a RootCtx, code: &'a str, options: Options) -> PRes<Program> {
    let source = code;
    let index = 0;
    let line_number = if code.len() > 0 { 1 } else { 0 };
    let line_start = 0;

    let start_index = index;
    let start_line_number = line_number;
    let start_line_start = line_start;
    let length = source.len();
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
        length: length,
        lookahead: lookahead,
        state: state,

        scanning: false,
        has_line_terminator: false,
        last_index: None,
        last_line_number: None,
        last_line_start: None,

        rest: source,

        root: root,
    };

    ctx.parse_program()
}

#[test]
fn it_works() {
    let root = RootCtx::new();
    parse(&root, include_str!("../tests/test.js"), Options).unwrap();
}
