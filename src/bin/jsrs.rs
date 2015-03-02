#![feature(env, libc, rustc_private, old_io, old_path, os)]

extern crate getopts;
extern crate js;
extern crate libc;

use getopts::{optflag,getopts,OptGroup};
use js::ast::Annotation;
use std::cmp;
use std::fmt;
use std::old_io::{self, IoResult, Reader, Writer};
use std::old_io::fs::File;
use std::os;
use std::sync::mpsc;
use std::thread;

#[derive(Copy,Debug)]
struct Loc {
    line: js::Pos,
    column: js::Pos,
}

#[derive(Copy,Debug)]
struct MyStart {
    start: Loc,
    start_index: js::Pos,
}

#[derive(Copy,Debug)]
struct MyAnnot {
    range: (js::Pos, js::Pos),
    start: Loc,
    end: Loc,
}

impl<'a> Annotation for MyAnnot {
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

    fn finish(&MyStart { start, start_index }: &MyStart, ctx: &js::Ctx<'a, MyAnnot, MyStart>) -> MyAnnot {

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

type Res<'a, Ann> = js::PRes<'a, js::ast::ScriptNode<'a, Ann>>;

struct Data<'a, Ann>(js::RootCtx, String, Option<Res<'a, Ann>>);

unsafe impl<'a, Ann> Send for Data<'a, Ann>
    where Ann: Send
    {}

struct Options {
    ast: bool,
    annotate: bool,
    run_destructors: bool,
}

fn parse<'a, I, Ann, Start>(_: &Options,
                            mut input: I,
                            data: &'a mut Option<Data<Ann>>
                           ) -> IoResult<()>
    where I: Reader + 'a,
          Ann: 'a,
          Ann: Annotation<Ctx=js::Ctx<'a, Ann, Start>, Start=Start>,
          //Ann: fmt::Debug,
{
    let string = try!(input.read_to_string());
    drop(input);
    let ctx = js::RootCtx::new();
    *data = Some(Data(ctx, string, None));
    let Data(ref ctx, ref string, ref mut res) = *data.as_mut().unwrap();
    *res = Some(js::parse::<Ann, Start>(ctx, string, &js::Options));
    Ok(())
}

fn display<'a, O, Ann, Start>(options: &Options, path: Option<&Path>,
                              data: IoResult<&Option<Data<'a, Ann>>>, output: &mut O
                             ) -> IoResult<()>
    where O: Writer,
          Ann: 'a,
          Ann: fmt::Debug,
          Ann: Annotation<Ctx=js::Ctx<'a, Ann, Start>, Start=Start>,
{
    try!(match path {
        Some(path) => write!(output, "{}: ", path.display()),
        None => write!(output, "<stdin>: ", )
    });

    match data {
        Ok(ref data) => {
            // Note: if the file was encoded successfully, it shouldn't be possible for the result
            // to be None here.  But even if it were (because of an accidental panic, say), that
            // should have already killed the display thread.
            let Data(_, _, ref result) = *data.as_ref().unwrap();
            let result = result.as_ref().unwrap();
            if options.ast {
                writeln!(output, "{:?}", result)
            } else {
                writeln!(output, "{:?}", result.as_ref().err())
            }
        },
        Err(e) => writeln!(output, "Error parsing input data!  {:?}", e)
    }
}

fn print_usage(program: &str, opts: &[OptGroup]) {
    let ref mut output = old_io::stdout();
    let brief = format!("Usage: {} [options]", program);
    write!(output, "{}", getopts::usage(&brief, opts)).unwrap();
}

fn run<Ann, Start>(options: &Options, matches: &getopts::Matches)
    where Ann: for<'a> Annotation<Ctx=js::Ctx<'a, Ann, Start>, Start=Start>,
          Ann: fmt::Debug,
          Ann: Send,
{
    let ref mut output = old_io::stdout();

    let ref mut error = old_io::stderr();

    let mut initial = matches.free.iter().map( |p| (Path::new(&*p), None::<Data<Ann>>) ).collect::<Vec<_>>();
    if matches.free.is_empty() {
        let mut data = None;
        let stdin = old_io::stdin();
        parse::<_, Ann, Start>(options, stdin, &mut data).unwrap();
        display(options, None, Ok(&data), output).unwrap();
    } else {
        let (tx, rx) = mpsc::channel();
        let _join_guards = initial
            .chunks_mut(cmp::max(matches.free.len() / os::num_cpus(), 1))
            .map( |chunk| {
                let tx = tx.clone();
                thread::scoped( move || {
                    for &mut (ref path, ref mut data) in chunk.iter_mut() {
                        let res = File::open(path).and_then( |file| parse(options, file, data));
                        try!(tx.send((path, res.and(Ok(data)))));
                    }
                    Ok::<_, mpsc::SendError<_>>(())
                } )
            } ).collect::<Vec<_>>();
        drop(tx);
        if let Err(e) = thread::scoped( move || {
                rx.iter()
                    .map( |(path, data)| display(options, Some(path), data.map( |&mut ref x| x), output) )
                    .collect::<IoResult<Vec<_>>>()
            } ).join() {
            writeln!(error, "There was an error executing the display task!  {:?}", e).unwrap()
        }
    }
    // We do this to avoid wasting time calling destructors, since the program is done anyway.
    if !options.run_destructors {
        unsafe { libc::exit(0 as libc::c_int); }
    }
}

pub fn main() {
    let mut args = std::env::args();

    let program = args.next().unwrap();

    let opts = [
        optflag("h", "help", "print this help menu"),
        optflag("", "ast", "print the successfully parsed AST"),
        optflag("", "annotate", "annotate each AST token"),
        optflag("", "run-destructors", "run destructors on exit (e.g. for use with Valgrind)")
    ];
    let matches = match getopts(&args.collect::<Vec<_>>(), &opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    let ref options = Options {
        ast: matches.opt_present("ast"),
        annotate: matches.opt_present("annotate"),
        run_destructors: matches.opt_present("run-destructors"),
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
