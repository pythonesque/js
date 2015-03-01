#![feature(env, rustc_private, old_io, old_path, os)]

extern crate getopts;
extern crate js;

use getopts::{optflag,getopts,OptGroup};
use std::cmp;
use std::old_io::{self, IoResult, Reader, Writer};
use std::old_io::fs::File;
use std::os;
use std::sync::mpsc;
use std::thread;

type Res<'a> = js::PRes<'a, js::ast::ScriptNode<'a, ()>>;

struct Data<'a>(js::RootCtx, String, Option<Res<'a>>);

unsafe impl<'a> Send for Data<'a> {}

struct Options {
    ast: bool,
}

fn parse<'a, I>(_: &Options, mut input: I, data: &'a mut Option<Data>) -> IoResult<()>
    where I: Reader + 'a
{
    let string = try!(input.read_to_string());
    drop(input);
    let ctx = js::RootCtx::new();
    *data = Some(Data(ctx, string, None));
    let Data(ref ctx, ref string, ref mut res) = *data.as_mut().unwrap();
    *res = Some(js::parse::<(), ()>(ctx, string, &js::Options));
    Ok(())
}

fn display<'a, O>(options: &Options, path: Option<&Path>,
                  data: IoResult<&Option<Data<'a>>>, output: &mut O
                 ) -> IoResult<()>
    where O: Writer
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

pub fn main() {
    let mut args = std::env::args();

    let program = args.next().unwrap();

    let ref mut output = old_io::stdout();

    let ref mut error = old_io::stderr();

    let opts = [
        optflag("h", "help", "print this help menu"),
        optflag("", "ast", "print the successfully parsed AST"),
    ];
    let matches = match getopts(&args.collect::<Vec<_>>(), &opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    let ref options = Options {
        ast: matches.opt_present("ast")
    };
    if matches.opt_present("h") {
        print_usage(&*program, &opts);
        return;
    }

    let mut initial = matches.free.iter().map( |p| (Path::new(&*p), None::<Data>) ).collect::<Vec<_>>();
    if matches.free.is_empty() {
        let mut data = None;
        let stdin = old_io::stdin();
        parse(options, stdin, &mut data).unwrap();
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
}
