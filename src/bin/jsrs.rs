#![feature(env, rustc_private, old_io, old_path, os)]

extern crate getopts;
extern crate js;

use getopts::{optflag,getopts,OptGroup};
use js::{Options, PRes, RootCtx};
use js::ast::{ScriptNode};
use std::cmp;
use std::old_io::{self, IoError, Reader, Writer};
use std::old_io::fs::File;
use std::os;
use std::thread;

type Res<'a> = PRes<'a, ScriptNode<'a, ()>>;

pub struct Data<'a>(RootCtx, String, Option<Res<'a>>);

unsafe impl<'a> Send for Data<'a> {}

fn parse<'a, I>(options: &Options, mut input: I, data: &'a mut Option<Data>) -> Result<&'a Res<'a>, IoError>
    where I: Reader + 'a
{
    let string = try!(input.read_to_string());
    drop(input);
    let ctx = RootCtx::new();
    *data = Some(Data(ctx, string, None));
    let Data(ref ctx, ref string, ref mut res) = *data.as_mut().unwrap();
    *res = Some(js::parse::<(), ()>(ctx, string, options));
    Ok(res.as_ref().unwrap())
}

fn display<'a, O>(path: Option<Path>, data: Option<Data<'a>>, output: &mut O)
    where O: Writer
{
    match path {
        Some(path) => write!(output, "{}: ", path.display()),
        None => write!(output, "<stdin>: ", )
    }.unwrap();
    match data {
        Some(Data(_, _, result)) => {
            // Note: if the file was encoded successfully, it shouldn't be possible for the result
            // to be None here.  But even if it were (because of an accidental panic, say), that
            // should have already killed the display thread.
            writeln!(output, "{:?}", result.unwrap()/*.is_ok()*//*.err()*/)
        },
        None => {
            writeln!(output, "No parse result found: check the file's encoding.")
        }
    }.unwrap();
}

fn print_usage(program: &str, _opts: &[OptGroup]) {
    let ref mut output = old_io::stdout();
    (writeln!(output, "Usage: {} [options]", program)).unwrap();
    (writeln!(output, "-h --help\tUsage")).unwrap();
}

pub fn main() {
    let mut args = std::env::args();

    let program = args.next().unwrap();

    let ref mut output = old_io::stdout();

    let opts = [
        optflag("h", "help", "print this help menu")
    ];
    let matches = match getopts(&args.collect::<Vec<_>>(), &opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    let ref options = Options;
    if matches.opt_present("h") {
        print_usage(&*program, &opts);
        return;
    }
    if matches.free.is_empty() {
        let mut data = None;
        let stdin = old_io::stdin();
        parse(options, stdin, &mut data).unwrap();
        display(None, data, output);
    } else {
        let mut initial = matches.free.iter().map( |p| (Path::new(&*p), None::<Data>) ).collect::<Vec<_>>();
        {
            initial
                .chunks_mut(cmp::max(matches.free.len() / os::num_cpus(), 1))
                .map( |chunk| thread::scoped( move || {
                for &mut (ref path, ref mut data) in chunk.iter_mut() {
                    if let Ok(file) = File::open(path) {
                        // If this fails, it will be detected at output time.
                        let _ = parse(options, file, data);
                    }
                }
            }) ).collect::<Vec<_>>();
        }
        for (path, data) in initial {
            display(Some(path), data, output);
        }
    }
}
