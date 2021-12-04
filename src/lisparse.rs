use std::collections::HashMap;
use regex::Regex;
use colored::Colorize;
use crate::lisparse::LispType::Atom;
// RE2 Number(R"(^-?\+?0|[1-9]\d*$)");
// RE2 Float(R"(^-?\d+\.\d+$)");

#[derive(Debug, Clone)]
pub enum Atomic {
    Number(i32),
    Float(f64),
    Symbol(String),  // TODO: refactor to &str
    Fun(fn(Vec<Atomic>)->Atomic)
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
            _ => match is_number(&content) {
                        1 => LispType::Atom(Atomic::Number(content.parse::<i32>().unwrap())),
                        2 => LispType::Atom(Atomic::Float(content.parse::<f64>().unwrap())),
                        _ => LispType::Atom(Atomic::Symbol(content.clone()))
                    }
        };
        tv.push(item);
    }
    LispType::List(tv)
}

pub fn is_func(s: &String, env_op: &HashMap<String, fn(Vec<Atomic>)->Atomic>) ->bool {
    return env_op.contains_key(s);
}

pub fn is_symbol(s: &String, env_symbol: &HashMap<String, Atomic>) ->bool {
    return env_symbol.contains_key(s);
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
    match expr {
        LispType::Atom(atom) => {
            match atom {
                Atomic::Symbol(sym) => {
                    if is_func(sym, env_op) {
                        return Atomic::Fun(env_op[sym]);
                    } else if is_symbol(sym, env_symbol) {
                        return env_symbol[sym].clone();
                    } else {
                        println!("{} {}",sym.green(),"is not defined!\n".red());
                        return Atomic::Number(-1);
                    }
                },
                _ => {  return atom.clone(); }
            };
        }
        LispType::List(list) => {
            if list.len()==0 {
                // TODO: deal with empty list
                println!("{}","Empty list!".green());
                return Atomic::Number(-1);
            }
            let result = eval(&list[0], env_symbol, env_op);
            match result {
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
                            println!("{}","Undefined keywords!".red());
                        }
                    }
                },
                Atomic::Fun(f) => {
                    return f(list[1..].into_iter().map(|subexpr| eval(subexpr, env_symbol, env_op)).collect());
                },
                _ => {
                    return result.clone();
                }
            }
        }
    }
    Atomic::Number(0)
}

pub fn init_env_symbol() ->HashMap<String, Atomic> {
    let mut env_symbol: HashMap<String, Atomic> = HashMap::new();
    env_symbol.insert(String::from("PI"), Atomic::Float(std::f64::consts::PI));
    env_symbol.insert(String::from("if"), Atomic::Symbol(String::from("if")));
    env_symbol.insert(String::from("define"), Atomic::Symbol(String::from("define")));
    env_symbol
}

pub fn init_env_op() ->HashMap<String, fn(Vec<Atomic>)->Atomic> {
    let mut env_op: HashMap<String, fn(Vec<Atomic>)->Atomic> = HashMap::new();
    env_op.insert(String::from("+"), add);
    env_op.insert(String::from("-"), minus);
    env_op.insert(String::from("*"), mul);
    env_op.insert(String::from("/"), div);
    env_op.insert(String::from(">"), gt);
    env_op.insert(String::from(">="), ge);
    env_op.insert(String::from("<"), lt);
    env_op.insert(String::from("<="), le);
    env_op.insert(String::from("="), eq);
    env_op.insert(String::from("/="), neq);
    env_op.insert(String::from("begin"), begin);
    env_op
}

/*********************************/
/**** Below are env functions ****/
/*********************************/

macro_rules! get_atom_value {
    ($atom:expr, $ret_type:ty) => {match $atom {
        Atomic::Number(n) => n as $ret_type,
        Atomic::Float(fp) => fp as $ret_type,
        _ => {
            println!("{}", "Operands should be of type i32 or f64".red());
            -1 as $ret_type
        }
    }};
}

pub fn add(args: Vec<Atomic>) ->Atomic {
    let mut res_n = 0;
    let mut res_fp = 0.0;
    let mut flag = false;
    for atom in args.iter(){
        match atom {
            Atomic::Number(n) => { res_n+=n; },
            Atomic::Float(fp) => { res_fp+=fp; flag=true; },
            _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
        }
    }
    if flag {
        return Atomic::Float(res_fp+(res_n as f64));
    }
    Atomic::Number(res_n)
}

pub fn minus(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Number(n - get_atom_value!(args[1],i32));
        },
        Atomic::Float(fp) => {
            return Atomic::Float(fp - get_atom_value!(args[1],f64));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn mul(args: Vec<Atomic>) ->Atomic {
    let mut res_n = 1;
    let mut res_fp = 1.0;
    let mut flag = false;
    for atom in args.iter(){
        match atom {
            Atomic::Number(n) => { res_n*=n; },
            Atomic::Float(fp) => { res_fp*=fp; flag=true; },
            _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
        }
    }
    if flag {
        return Atomic::Float(res_fp+(res_n as f64));
    }
    Atomic::Number(res_n)
}

pub fn div(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Float(n as f64 / get_atom_value!(args[1],f64));
        },
        Atomic::Float(fp) => {
            return Atomic::Float(fp / get_atom_value!(args[1],f64));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn gt(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Number(i32::from((n as f64) > get_atom_value!(args[1],f64)));
        },
        Atomic::Float(fp) => {
            return Atomic::Number(i32::from(fp > get_atom_value!(args[1],f64)));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(0)
}

pub fn lt(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Number(i32::from((n as f64) < get_atom_value!(args[1],f64)));
        },
        Atomic::Float(fp) => {
            return Atomic::Number(i32::from(fp < get_atom_value!(args[1],f64)));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn ge(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Number(i32::from((n as f64) >= get_atom_value!(args[1],f64)));
        },
        Atomic::Float(fp) => {
            return Atomic::Number(i32::from(fp >= get_atom_value!(args[1],f64)));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn le(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Number(i32::from((n as f64) <= get_atom_value!(args[1],f64)));
        },
        Atomic::Float(fp) => {
            return Atomic::Number(i32::from(fp <= get_atom_value!(args[1],f64)));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn eq(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Number(i32::from((n as f64) == get_atom_value!(args[1],f64)));
        },
        Atomic::Float(fp) => {
            return Atomic::Number(i32::from(fp == get_atom_value!(args[1],f64)));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn neq(args: Vec<Atomic>) ->Atomic {
    match args[0] {
        Atomic::Number(n) => {
            return Atomic::Number(i32::from((n as f64) != get_atom_value!(args[1],f64)));
        },
        Atomic::Float(fp) => {
            return Atomic::Number(i32::from(fp != get_atom_value!(args[1],f64)));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn begin(args: Vec<Atomic>) ->Atomic {
    return args.last().unwrap().clone();
}

pub fn _max(arg0: &Atomic, arg1: &Atomic) -> Atomic {
    match (arg0, arg1) {
        (Atomic::Number(n1), Atomic::Number(n2)) => {
            return Atomic::Number(*n1.max(n2));
        },
        (Atomic::Number(n1),Atomic::Float(n2)) => {
            return Atomic::Float(n2.max(*n1 as f64));
        }
        (Atomic::Float(n1),Atomic::Number(n2)) => {
            return Atomic::Float(n1.max(*n2 as f64));
        },
        (Atomic::Float(n1),Atomic::Float(n2)) => {
            return Atomic::Float(n1.max(*n2));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn max(args: Vec<Atomic>) ->Atomic {
    assert!(args.len()>1);
    let mut res: Atomic = args[0].clone();
    for i in 1..args.len() {
        res = _max(&res, &args[i]);
    }
    return res.clone();
}

pub fn _min(arg0: &Atomic, arg1: &Atomic) ->Atomic {
    match (arg0, arg1) {
        (Atomic::Number(n1), Atomic::Number(n2)) => {
            return Atomic::Number(*n1.min(n2));
        },
        (Atomic::Number(n1),Atomic::Float(n2)) => {
            return Atomic::Float(n2.min(*n1 as f64));
        }
        (Atomic::Float(n1),Atomic::Number(n2)) => {
            return Atomic::Float(n1.min(*n2 as f64));
        },
        (Atomic::Float(n1),Atomic::Float(n2)) => {
            return Atomic::Float(n1.min(*n2));
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    Atomic::Number(-1)
}

pub fn min(args: Vec<Atomic>) ->Atomic {
    assert!(args.len()>1);
    let mut res: Atomic = args[0].clone();
    for i in 1..args.len() {
        res = _min(&res,&args[i]);
    }
    return res.clone();
}