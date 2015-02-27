#![feature(env, rustc_private, old_io, old_path, os)]

extern crate getopts;
extern crate js;

use getopts::{optflag,getopts,OptGroup};
use js::{Options, PRes, RootCtx};
use js::ast::{ScriptNode};
use std::cmp;
use std::old_io::{self, Reader, Writer};
use std::old_io::fs::File;
use std::os;
use std::thread;

type Res<'a> = PRes<'a, ScriptNode<'a, ()>>;

pub struct Data<'a>(RootCtx, String, Option<Res<'a>>);

unsafe impl<'a> Send for Data<'a> {}

fn parse<'a, I>(options: &Options, mut input: I, data: &'a mut Option<Data>) -> &'a Res<'a>
    where I: Reader + 'a
{
    let string = input.read_to_string().unwrap();
    drop(input);
    let ctx = RootCtx::new();
    *data = Some(Data(ctx, string, None));
    let Data(ref ctx, ref string, ref mut res) = *data.as_mut().unwrap();
    *res = Some(js::parse::<(), ()>(ctx, string, options));
    res.as_ref().unwrap()
}

fn display<'a, O>(path: Option<Path>, data: Option<Data<'a>>, output: &mut O)
    where O: Writer
{
    let Data(_, _, result) = data.unwrap();
    let result = result.unwrap();
    /*(writeln!(output, "{:?}", result)).unwrap();*/
    match path {
        Some(path) => write!(output, "{}: ", path.display()),
        None => write!(output, "<stdin>: ", )
    }.unwrap();
    writeln!(output, "{:?}", result/*.is_ok()*//*.err()*/).unwrap();
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
        parse(options, stdin, &mut data);
        display(None, data, output);
    } else {
        let mut initial = matches.free.iter().map( |p| (Path::new(&*p), None::<Data>) ).collect::<Vec<_>>();
        {
            initial
                .chunks_mut(cmp::max(matches.free.len() / os::num_cpus(), 1))
                .map( |chunk| thread::scoped( move || {
                for &mut (ref path, ref mut data) in chunk.iter_mut() {
                    let file = File::open(path).unwrap();
                    parse(options, file, data);
                }
            }) ).collect::<Vec<_>>();
        }
        for (path, data) in initial {
            display(Some(path), data, output);
        }
    }
}
