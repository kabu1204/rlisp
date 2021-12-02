use std::collections::HashMap;
use colored::Colorize;
use dialoguer::Input;
use crate::lisparse::cvt_to_nested_expression;

mod lisparse;

fn main() {
    println!("{}\n{}","This is a Lisp interpreter with Rust runtime.".green(),"Press C-c to exit.".green());
    let mut _buf = String::new();
    let mut Env_Symbols:HashMap<String, lisparse::Atomic> = HashMap::new();
    Env_Symbols.insert("PI".to_string(),lisparse::Atomic::Float(std::f64::consts::PI));
    loop{
        // Use RustyLine instead
        _buf = Input::new().with_prompt("> ").interact_text().unwrap();
        println!("{:?}", _buf);
        let v = lisparse::split_cmd_to_vec(&_buf);
        for s in v.iter() {
            println!("{}",s);
        }
        println!("{:?}",cvt_to_nested_expression(&v[..],&mut(0 as usize)));
    }
}