#![feature(env, rustc_private, old_io, old_path)]

extern crate getopts;
extern crate js;

use getopts::{optflag,getopts,OptGroup};
use js::{Options, RootCtx};
use std::old_io::{self, Reader, Writer};
use std::old_io::fs::File;

fn parse<I, O>(options: &Options, mut input: I,  output: &mut O)
    where I: Reader, O: Writer {
    let string = input.read_to_string().unwrap();
    drop(input);
    let ref ctx = RootCtx::new();
    (writeln!(output, "{:?}", js::parse::<()>(ctx, &string, options))).unwrap();
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
        parse(options, old_io::stdin(), output)
    } else {
        for path in matches.free.iter().map ( |p| Path::new(&*p) ) {
            let file = File::open(&path).unwrap();
            parse(options, file, output);
        }
    }
}
