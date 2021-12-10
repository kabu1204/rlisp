use colored::Colorize;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use crate::lisparse::{Eval};

mod lisparse;

fn main() {
    println!("{}\n{}","This is a Lisp interpreter with Rust runtime.".green(),"Press C-c to exit.".green());
    let mut _buf = Ok(String::new());
    let mut env = Box::new(lisparse::init_env());
    let mut rl = Editor::<()>::new();
    if rl.load_history("input_history.txt").is_err(){}
    loop {
        _buf = rl.readline(&">> ".green());
        match _buf {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("{}",Eval(&line, &mut env));
            },
            Err(ReadlineError::Interrupted) => {
                println!("C-c");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("C-d");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("input_history.txt").unwrap();
}
