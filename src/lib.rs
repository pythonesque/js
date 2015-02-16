use ast::Program;

use std::collections::HashSet;
use std::default::Default;

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

#[derive(Default)]
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
    lookahead: Option<&'a str>,
    state: State<'a>,

    scanning: bool,
    has_line_terminator: bool,
    last_index: Option<Pos>,
    last_line_number: Option<Pos>,
    last_line_start: Option<Pos>,
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
    UnexpectedToken
}

pub type PRes<T> = Result<T, Error>;

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

impl<'a> Ctx<'a> {
    pub fn skip_single_line_comment(&mut self, offset: usize) {
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

    pub fn skip_multi_line_comment(&mut self) -> PRes<()> {
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

    pub fn skip_comment(&mut self) -> PRes<()> {
        self.has_line_terminator = false;

        let mut start = (self.index == 0);
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
            } else if (ch == '<') {
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

    pub fn peek(&mut self) -> PRes<()> {
        self.scanning = true;

        try!(self.skip_comment());

        self.last_index = Some(self.index);
        self.last_line_number = Some(self.line_number);
        self.last_line_start = Some(self.line_start);

        self.start_index = self.index;
        self.start_line_number = self.line_number;
        self.start_line_start = self.line_start;

        //self.lookahead = /* extra.tokens */ self.advance();

        self.scanning = false;

        Ok(())
    }

    pub fn parse_program(&mut self) -> PRes<Program> {
        try!(self.peek());
        Ok(Program)
    }
}


pub fn parse<'a>(code: &'a str, options: Options) -> PRes<Program> {
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
        start_line_number: line_number,
        start_line_start: line_start,
        length: length,
        lookahead: lookahead,
        state: state,
        
        rest: source,

        .. Default::default()
    };

    ctx.parse_program()
}

#[test]
fn it_works() {
    parse(include_str!("../tests/test.js"), Options).unwrap();
}
