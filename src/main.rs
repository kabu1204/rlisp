use colored::Colorize;
use dialoguer::Input;

mod lisparse;

fn main() {
    println!("{}\n{}","This is a Lisp interpreter with Rust runtime.".green(),"Press C-c to exit.".green());
    let mut _buf = String::new();
    let mut env = Box::new(lisparse::init_env());
    loop{
        // Use RustyLine instead
        _buf = Input::new().with_prompt("> ").interact_text().unwrap();
        println!("{:?}", _buf);
        let v = lisparse::split_cmd_to_vec(&_buf);
        let expr = lisparse::cvt_to_nested_expression(&v[..],&mut(0 as usize));
        let result = lisparse::eval(&expr, &mut env);
        println!("{:?}",result);
    }
}