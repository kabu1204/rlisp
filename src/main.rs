use colored::Colorize;
use dialoguer::Input;
use crate::lisparse::{Eval};

mod lisparse;

fn main() {
    println!("{}\n{}","This is a Lisp interpreter with Rust runtime.".green(),"Press C-c to exit.".green());
    let mut _buf = String::new();
    let mut env = Box::new(lisparse::init_env());
    loop{
        // Use RustyLine instead
        _buf = Input::new().with_prompt("> ").interact_text().unwrap();
        // println!("{:?}", _buf);
        let result = Eval(&_buf, &mut env);
        println!("{:?}",result);
    }
}