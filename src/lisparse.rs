use std::collections::HashMap;
use regex::Regex;
use colored::Colorize;
// RE2 Number(R"(^-?\+?0|[1-9]\d*$)");
// RE2 Float(R"(^-?\d+\.\d+$)");

#[derive(Debug, Clone)]
pub enum Atomic {
    Number(i32),
    Float(f64),
    Symbol(String)  // TODO: refactor to &str
}

#[derive(Debug, Clone)]
pub enum LispType {
    Atom(Atomic),
    List(Vec<LispType>)
}

pub fn split_cmd_to_vec(cmd: &str) -> Vec<String> {
    let expanded_cmd = &cmd.trim().replace("(", "( ").replace(")", " )")[..];
    println!("Expanded command:{}",expanded_cmd);
    let splited_cmd: Vec<String> = expanded_cmd.split_ascii_whitespace()
                                                .map(str::to_string)
                                                .collect();
    return splited_cmd;
}

pub fn cvt_to_nested_expression(splited_cmd: &[String], idx: &mut usize) ->LispType {
    let mut tv: Vec<LispType> = Vec::new();
    while *idx < splited_cmd.len() {
        let content = &splited_cmd[*idx];
        *idx += 1;
        let item = match &content[..] {
            "(" => cvt_to_nested_expression(splited_cmd, idx),
            ")" => {return LispType::List(tv);},
            _ => if let true = is_symbol(&content){
                    LispType::Atom(Atomic::Symbol(content.clone()))
                }
                else {
                    match is_number(&content) {
                        1 => LispType::Atom(Atomic::Number(content.parse::<i32>().unwrap())),
                        2 => LispType::Atom(Atomic::Float(content.parse::<f64>().unwrap())),
                        _ => {
                            println!("Undefined symbol:{}", content);
                            LispType::Atom(Atomic::Number(0))
                        }
                    }
                }
        };
        tv.push(item);
    }
    LispType::List(tv)
}

pub fn is_symbol(s: &String) ->bool {
    true
}

pub fn is_number(s: &String) ->i32 {
    if Regex::new(r"^-?\+?0|[1-9]\d*$").unwrap().is_match(&s){
        return 1;
    }
    else if Regex::new(r"^-?\d+\.\d+$").unwrap().is_match(&s) {
        return 2;
    }
    0
}

pub fn eval(expr: &LispType, env_symbol: &mut HashMap<String, Atomic>,
                            env_op: &mut HashMap<String, fn(Vec<Atomic>)->Atomic>) -> Atomic {
    let _if = Atomic::Symbol(String::from("if"));
    let _def = Atomic::Symbol(String::from("define"));
    match expr {
        LispType::Atom(atom) => {
            match atom {
                Atomic::Symbol(sym) => { return env_symbol[sym].clone(); },
                _ => {  return atom.clone(); }
            };
        }
        LispType::List(list) => {
            if list.len()==0 {
                // TODO: deal with empty list
                println!("{}","Empty list!".green());
                return Atomic::Number(-1);
            }
            match eval(&list[0], env_symbol, env_op) {
                Atomic::Symbol(op_name) => {
                    match &op_name[..] {
                        "if" => {
                            // TODO: can be expanded to
                            // 1.(if test then...)
                            // 2.(if test then else...)
                            if let Atomic::Number(1) = eval(&list[1], env_symbol, env_op) {
                                return eval(&list[2], env_symbol, env_op);
                            } else {
                                return eval(&list[3], env_symbol, env_op);
                            }
                        },
                        "define" => {
                            let symbol = eval(&list[1], env_symbol, env_op);
                            if let Atomic::Symbol(symbol_name) = symbol {
                                let value = eval(&list[2], env_symbol, env_op);
                                env_symbol.insert(symbol_name, value);
                                // why not working? env_symbol.insert(symbol_name, eval(&list[2], env_symbol, env_op));
                            } else {
                                println!("{}","Not a valid symbol name!".red());
                            }
                        }
                        _ => {
                            let f = env_op[&op_name];
                            return f(list[1..].into_iter().map(|subexpr| eval(subexpr, env_symbol, env_op)).collect());
                        }
                    }
                },
                _ => { println!("{}","Undefined behavior".red()); }
            }
            for sub_expr in list.iter() {

            }
        }
    }
    Atomic::Number(-1)
}

pub fn add(args: Vec<Atomic>) ->Atomic {
    let mut res_n = 0;
    let mut res_fp = 0.0;
    let mut flag = false;
    for atom in args.iter(){
        match atom {
            Atomic::Number(n) => { res_n+=n; },
            Atomic::Float(fp) => { res_fp+=fp; flag=true; },
            _ => {
                println!("{}", "Operands should be of type i32 or f64".red());
            }
        }
    }
    if flag {
        return Atomic::Float(res_fp+(res_n as f64));
    }
    Atomic::Number(res_n)
}