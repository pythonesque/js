#![feature(exit_status, libc, rustc_private, scoped)]

extern crate getopts;
extern crate js;
extern crate libc;
extern crate num;
extern crate num_cpus;

use getopts::{getopts, optflag, optopt, OptGroup};
use js::ast::Annotation;
use std::cmp;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{self, BufWriter, Read, Write};
use num::NumCast;
use std::path::Path;
use std::sync::mpsc;
use std::thread;

#[derive(Clone,Copy,Debug)]
struct Loc {
    line: js::Pos,
    column: js::Pos,
}

#[derive(Clone,Copy,Debug)]
struct MyStart {
    start: Loc,
    start_index: js::Pos,
}

#[derive(Clone,Copy,Debug)]
struct MyAnnot {
    range: (js::Pos, js::Pos),
    start: Loc,
    end: Loc,
}

type Error<'a> = js::Error<Box<js::Token<js::ast::Tok<'a>>>>;

impl<'a> Annotation<'a> for MyAnnot {
    type Ctx = js::Ctx<'a, MyAnnot, MyStart>;
    type Start = MyStart;

    fn start(ctx: &js::Ctx<'a, MyAnnot, MyStart>) -> MyStart {
        let start = MyStart {
            start_index: ctx.start_index,
            start: Loc { line: ctx.start_line_number, column: ctx.start_index - ctx.start_line_start + 1 },
        };
        //println!("Starting: {:?}", start);
        start
    }

    fn finish(&MyStart { start, start_index }: &MyStart,
              ctx: &js::Ctx<'a, MyAnnot, MyStart>,
             ) -> MyAnnot {

        let end = match *ctx {
            js::Ctx {
                last_line_number: Some(last_line_number),
                last_index: Some(last_index),
                last_line_start: Some(last_line_start),
                ..
            } => MyAnnot {
                range: (start_index, last_index),
                start: start,
                end: Loc { line: last_line_number, column: last_index - last_line_start + 1 },
            },
            _ => MyAnnot {
                range: (start_index, start_index),
                start: start,
                end: Loc { line: start.line, column: start.column },
            }
        };
        //println!("Ending: {:?}", end);
        end
    }
}

#[derive(Clone,Copy,PartialEq)]
enum Mode {
    Unchecked,
    Flow,
}

struct Res<'a, Ann> {
    mode: Mode,
    result: js::PRes<'a, js::ast::ScriptNode<'a, Ann>>,
}

struct Data<'a, Ann>(js::RootCtx, String, Option<Res<'a, Ann>>);

#[derive(Clone,Copy,PartialEq)]
enum Flow {
    Check,
    Ignore,
    Only,
}

struct Options {
    ast: bool,
    annotate: bool,
    run_destructors: bool,
    only_errors: bool,
    flow: Option<Flow>,
    unbuffered: bool,
    shebang: bool,
    names: bool,
}

fn parse<'a, 'b, Ann, Start>
        (options: &Options,
         ctx: &'a js::RootCtx, string: &'a str, res: &mut Option<Res<'a, Ann>>)
    where Ann: Annotation<'b, Ctx=js::Ctx<'a, Ann, Start>, Start=Start>,
          //Ann: fmt::Debug,
{
    let (string, index) = if options.shebang && string.starts_with("#!") {
        // The reason we have to check for overflow here is that we need to do a conversion to
        // Pos from usize, which could potentially fail.  Normally this is not necessary because
        // the parser will check for overflow itself, but in this case we are doing preprocessing
        // so we can't rely on that.
        let (string, index) = match string.char_indices()
                                          .enumerate()
                                          .find( |&(_, (_, ch))| ch == '\n') {
            Some((index, (byte_index, _))) => (&string[byte_index ..], (index - 1)),
            None => ("", string.chars().count())
        };
        match NumCast::from(index) {
            Some(index) => (string, index),
            None => {
                *res = Some(Res {
                    result: Err(js::Error::PosOverflow),
                    mode: Mode::Unchecked,
                });
                return;
            }
        }
    } else {
        (string, 0)
    };
    *res = Some(match options.flow {
        Some(_) => {
            let mut mode = None;
            let result = js::parse::<Ann, Start, _>(ctx, string, js::Options {
                add_comment: |js::Token { ty, .. }| {
                    if let None = mode {
                        mode = Some(if match ty {
                            js::Comment::Block { comment , .. } => comment,
                            js::Comment::Line(comment) => comment,
                        }.split_whitespace().any( |word| match word {
                            "@flow" | "@lanetixFlowInterface" => true, _ => false } ) {
                            Mode::Flow
                        } else {
                            Mode::Unchecked
                        });
                    }
                },
                index: index,
                line_number: 0,
                line_start: 0,
            });
            Res { mode: mode.unwrap_or(Mode::Unchecked), result: result }
        },
        None => {
            Res {
                result: js::parse::<Ann, Start, _>(ctx, string, js::Options {
                    add_comment: |_| {},
                    index: index,
                    line_number: 0,
                    line_start: 0,
                }),
                mode: Mode::Unchecked,
            }
        }
    });
}

fn display<'a, O, Ann, Start>
          (options: &Options,
           path: Option<&Path>,
           result: io::Result<&'a Option<Res<Ann>>>,
           output: &mut O,
          ) -> io::Result<io::Result<Result<(), &'a Error<'a>>>>
    where O: Write,
          Ann: 'a,
          Ann: fmt::Debug,
          Ann: Annotation<'a, Ctx=js::Ctx<'a, Ann, Start>, Start=Start>,
{
    match result {
        Ok(ref result) => {
            // Note: if the file was encoded successfully, it shouldn't be possible for the result
            // to be None here.  But even if it were (because of an accidental panic, say), that
            // should have already killed the display thread.
            let result = result.as_ref().unwrap();
            if (!options.only_errors || result.result.is_err()) &&
               !(options.flow == Some(Flow::Ignore) && result.mode == Mode::Flow) &&
               (options.flow != Some(Flow::Only) || result.mode == Mode::Flow) {
                try!(match path {
                    Some(path) => write!(output, "{}: ", path.display()),
                    None => write!(output, "<stdin>: ", )
                });
                if options.ast {
                    writeln!(output, "{:?}", result.result)
                } else {
                    writeln!(output, "{:?}", result.result.as_ref().err())
                }.and_then( |_| if options.unbuffered { output.flush() } else { Ok(()) })
                 .and(Ok(Ok(result.result.as_ref().and(Ok(())))))
            } else {
                Ok(Ok(Ok(())))
            }
        },
        Err(e) => {
            try!(match path {
                Some(path) => write!(output, "{}: ", path.display()),
                None => write!(output, "<stdin>: ", )
            });
            writeln!(output, "Error parsing input data!  {:?}", e)
                .and_then(|_| if options.unbuffered { output.flush() } else { Ok(()) })
                .and(Ok(Err(e)))
        }
    }
}

fn print_usage(program: &str, opts: &[OptGroup]) {
    let ref mut output = io::stdout();
    let brief = format!("Usage: {} [options]", program);
    write!(output, "{}", getopts::usage(&brief, opts)).unwrap();
}

fn run<Ann, Start>(options: &Options, matches: &getopts::Matches)
    where Ann: for<'a> Annotation<'a, Ctx=js::Ctx<'a, Ann, Start>, Start=Start>,
          Ann: fmt::Debug,
          Ann: Send,
{
    let ref mut output = io::stdout();

    let ref mut error = io::stderr();

    let mut names;
    let mut initial = matches.free
        .iter()
        .map( |p| (Path::new(&*p), Data(js::RootCtx::new(), String::new(), None::<Res<Ann>>)) )
        .collect::<Vec<_>>();

    if options.names {
        names = String::new();
        io::stdin().read_to_string(&mut names).unwrap();
        initial.extend(names
            .lines()
            .map( |p| (Path::new(&*p), Data(js::RootCtx::new(), String::new(), None)) ));
    }

    if initial.is_empty() {
        let ctx = js::RootCtx::new();
        let mut string = String::new();
        io::stdin().read_to_string(&mut string).unwrap();
        let mut res = None;
        parse::<Ann, Start>(options, &ctx, &string, &mut res);
        let ref mut output = BufWriter::new(output.lock());
        match display(options, None, Ok(&res), output) {
            Ok(r) => match r {
                Ok(r) => if let Err(_) = r { env::set_exit_status(1) },
                Err(_) => env::set_exit_status(2),
            },
            Err(_) => env::set_exit_status(3),
        }
    } else {
        let chunk_size = cmp::max(initial.len() / num_cpus::get(), 1);
        let (tx, rx) = mpsc::channel();
        let _join_guards =
            initial
            .chunks_mut(chunk_size)
            .into_iter()
            .map( |chunk| {
                let tx = tx.clone();
                thread::scoped( move || {
                    for &mut (ref path, Data(ref mut ctx, ref mut string, ref mut result)) in chunk {
                        let res = File::open(path)
                            .and_then( |mut file| file.read_to_string(&mut *string));
                        let ctx = &mut *ctx;
                        let string = &*string;
                        try!(tx.send((path, match res {
                            Ok(_) => {
                                parse(options, ctx, string, result);
                                Ok((string, result))
                            },
                            Err(e) => Err(e)
                        })));
                    }
                    Ok::<_, mpsc::SendError<_>>(())
                } )
            } ).collect::<Vec<_>>();
        drop(tx);
        match thread::scoped( move || {
            let ref mut output = BufWriter::new(output.lock());
                rx.iter()
                    .map( |(path, data)|
                          display(options, Some(path), data.map( |(_, &mut ref x)| x), output) )
                    .collect::<io::Result<Vec<_>>>()
            } ).join() {
            Ok(v) => {
                match v.into_iter().collect::<io::Result<Vec<_>>>() {
                    Ok(v) => if let Err(_) = v.into_iter().collect::<Result<Vec<()>, &Error>>() {
                        env::set_exit_status(1);
                    },
                    Err(_) => env::set_exit_status(2)
                }
            }
            Err(e) => {
                writeln!(error, "There was an error executing the display task!  {:?}", e).unwrap();
                env::set_exit_status(3);
            }
        }
    }
    // We do this to avoid wasting time calling destructors, since the program is done anyway.
    if !options.run_destructors {
        unsafe { libc::exit(env::get_exit_status() as libc::c_int); }
    }
}

pub fn main() {
    let mut args = env::args();

    let program = args.next().unwrap();

    let opts = [
        optflag("h", "help", "print this help menu"),
        optflag("", "ast", "print the successfully parsed AST"),
        optflag("", "only-errors", "only display entries if there was an error"),
        optflag("", "annotate", "annotate each AST token"),
        optflag("", "run-destructors", "run destructors on exit (e.g. for use with Valgrind)"),
        optopt("", "flow", "detect @flow annotations (no [default], yes, only, ignore)", "FLOW"),
        optflag("", "unbuffered", "don't buffer output (it is always buffered per line)"),
        optflag("", "shebang", "ignore #! at start of file"),
        optflag("", "names", "interpret stdin as file names to parse"),
    ];
    let matches = match getopts(&args.collect::<Vec<_>>(), &opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    let ref options = Options {
        ast: matches.opt_present("ast"),
        annotate: matches.opt_present("annotate"),
        run_destructors: matches.opt_present("run-destructors"),
        only_errors: matches.opt_present("only-errors"),
        flow: matches.opt_str("flow").and_then( |flow| match &*flow {
            "yes" => Some(Flow::Check),
            "only" => Some(Flow::Only),
            "ignore" => Some(Flow::Ignore),
            "no" => None,
            opt => panic!("Unrecognized argument `{}` for option `flow`", opt)
        }),
        unbuffered: matches.opt_present("unbuffered"),
        shebang: matches.opt_present("shebang"),
        names: matches.opt_present("names"),
    };
    if matches.opt_present("h") {
        print_usage(&*program, &opts);
        return;
    }

    if options.annotate {
        run::<MyAnnot, MyStart>(&options, &matches)
    } else {
        run::<(), ()>(&options, &matches)
    }
}
