use std::{collections::HashMap, fmt};
use std::fmt::{Formatter};
use regex::Regex;
use colored::Colorize;

macro_rules! lisp_atom {
    ($var:expr, $atom_type:ident) => {
        LispType::Atom(Atomic::$atom_type($var))
    };
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum Atomic {
    Number(i32),
    Float(f64),
    Symbol(String),
    Fun(fn(Vec<LispType>)->LispType),
    Proc(Box<Proc>),
    nil,
    t
}

impl fmt::Display for Atomic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Atomic::Number(n) => write!(f, "{}", n),
            Atomic::Float(fp) => write!(f, "{}", fp),
            Atomic::Symbol(sym) => write!(f, "{}", sym),
            Atomic::nil => write!(f, "nil"),
            Atomic::t => write!(f, "t"),
            Atomic::Fun(_f) => write!(f, "{:?}", _f),
            Atomic::Proc(uf) => write!(f, "{:?}", uf)
        }
    }
}

impl std::cmp::PartialEq for Atomic {
    fn eq(&self, other: &Self) -> bool {
        match (&self, &other) {
            (&Atomic::Number(n1), &Atomic::Number(n2)) => n1==n2,
            (&Atomic::Float(fp1), &Atomic::Float(fp2)) => fp1==fp2,
            (&Atomic::Symbol(s1), &Atomic::Symbol(s2)) => s1==s2,
            (&Atomic::nil, &Atomic::nil) => true,
            (&Atomic::Fun(f1), &Atomic::Fun(f2)) => f1==f2,
            (&Atomic::t, &Atomic::t) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LispType {
    Atom(Atomic),
    List(Vec<LispType>)
}

impl fmt::Display for LispType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", expr2str(self))
    }
}

pub fn split_cmd_to_vec(cmd: &str) -> Vec<String> {
    let expanded_cmd = &cmd.trim().replace("(", "( ")
                                        .replace(")", " )")
                                        .replace("'(", "'( ")[..];
    let splited_cmd: Vec<String> = expanded_cmd.split_ascii_whitespace()
                                                .map(str::to_string)
                                                .collect();
    let mut splited_cmd_: Vec<String> = Vec::new();
    let mut bracket_v: Vec<&str> = Vec::new();
    for v in splited_cmd{
        match v.as_str() {
            "'(" => {
                splited_cmd_.push(String::from("("));
                splited_cmd_.push(String::from("quote"));
                splited_cmd_.push(String::from("("));
                bracket_v.push("'(");
            },
            "(" => {
                splited_cmd_.push(String::from("("));
                bracket_v.push("(");
            },
            ")" => {
                match bracket_v.pop() {
                    Some("(") => splited_cmd_.push(String::from(")")),
                    Some("'(") => {
                        splited_cmd_.push(String::from(")"));
                        splited_cmd_.push(String::from(")"));
                    }
                    _ => {}
                }
            }
            _ => {
                if v.starts_with("'"){
                    splited_cmd_.push(String::from("("));
                    splited_cmd_.push(String::from("quote"));
                    splited_cmd_.push(String::from(&v[1..]));
                    splited_cmd_.push(String::from(")"));
                } else {
                    splited_cmd_.push(v);
                }
            }
        }
    }
    return splited_cmd_;
}

pub fn cvt_to_nested_expression(splited_cmd: &[String], idx: &mut usize, depth: i32) ->LispType {
    let mut tv: Vec<LispType> = Vec::new();
    while *idx < splited_cmd.len() {
        let content = &splited_cmd[*idx];
        *idx += 1;
        let item = match &content[..] {
            "(" => cvt_to_nested_expression(splited_cmd, idx, depth+1),
            ")" => {return LispType::List(tv);},
            "nil" => { LispType::Atom(Atomic::nil) },
            "t" => { LispType::Atom(Atomic::t) },
            _ => match is_number(&content) {
                        1 => LispType::Atom(Atomic::Number(content.parse::<i32>().unwrap())),
                        2 => LispType::Atom(Atomic::Float(content.parse::<f64>().unwrap())),
                        _ => LispType::Atom(Atomic::Symbol(content.clone()))
                    }
        };
        tv.push(item);
    }
    if depth==0 { tv.last().unwrap().clone() } else { LispType::List(tv) }
}

pub fn expr2str(expr: &LispType) ->String {
    match expr {
        LispType::Atom(atom) => { format!("{}", atom) },
        LispType::List(list) => {
            let mut stringv: Vec<String> = Vec::new();
            for elem in list{
                stringv.push(expr2str(elem));
            }
            format!("({})", stringv.join(" "))
        }
    }
}

pub fn is_number(s: &String) ->i32 {
    if Regex::new(r"^-?\d+$").unwrap().is_match(&s){
        return 1;
    }
    else if Regex::new(r"^[-+]?[0-9]*\.?[0-9]+$").unwrap().is_match(&s) {
        return 2;
    }
    0
}

#[derive(Debug, Clone)]
pub struct Proc {
    params: Vec<String>,
    expr: LispType,
    parent_env: Box<Env>
}

impl Proc {
    fn new(params: Vec<String>, expr:LispType, parent_env: Box<Env>) ->Proc {
        return Proc{params, expr, parent_env};
    }
    fn run(&mut self, args: Vec<LispType>, parent_env: Box<Env>) ->LispType{
        if args.len()!=self.params.len() {
            println!("{}","Unmatched arguments with parameters".red());
        } else {
            let mut child_env = Box::new(Env::new(Some(parent_env)));
            for i in 0..args.len(){
                child_env.add_symbol(&self.params[i], &args[i]);
            }
            return eval(&self.expr, &mut child_env);
        }
        LispType::Atom(Atomic::nil)
    }
}

#[derive(Debug,Clone)]
pub struct Env {
    local_env: HashMap<String, LispType>,
    parent_env: Option<Box<Env>>
}

impl Env {
    fn new(_parent_env: Option<Box<Env>>) ->Env{
        return Env{
            local_env: HashMap::new(),
            parent_env: _parent_env
        }
    }
    fn add_symbol(&mut self, sym: &String, value: &LispType) {
        use std::collections::hash_map::Entry;
        match self.local_env.entry(sym.clone()) {
            Entry::Occupied(_) => {
                println!("{}:{}","You can't define a symbol twice".red(),sym.green());
            },
            Entry::Vacant(entry) => {
                entry.insert(value.clone());
            }
        }
    }
    fn set_symbol(&mut self, sym: &String, value: &LispType) {
        use std::collections::hash_map::Entry;
        match self.local_env.entry(sym.clone()) {
            Entry::Occupied(mut entry) => {
                entry.insert(value.clone());
            },
            Entry::Vacant(_) => {
                println!("{}","Assigning to a undefined symbol is not allowed".red());
            }
        }
    }
    fn lookup_symbol(&self, s: &String) ->Result<LispType, &'static str>{
        return if self.local_env.contains_key(s) {
            Ok(self.local_env[s].clone())
        } else {
            match &(self.parent_env) {
                None => Err("Keyword Error: Undefined symbol"),
                Some(ptr) => ptr.lookup_symbol(s)
            }
        }
    }
}

#[allow(non_snake_case)]
pub fn Eval(cmd: &str, env: &mut Box<Env>) ->LispType{
    let v = split_cmd_to_vec(cmd);
    let expr = cvt_to_nested_expression(&v[..],&mut(0 as usize), 0);
    eval(&expr, env)
}

pub fn eval(expr: &LispType, env:&mut Box<Env>) ->LispType {
    match expr {
        LispType::Atom(atom) => {
            match atom {
                Atomic::Symbol(sym) => {
                    if let Ok(res) = env.lookup_symbol(sym){
                        return res;
                    } else {
                        println!("{} {}", sym.green(), "is not defined!".red());
                    }
                },
                _ => {  return expr.clone(); }
            };
        }
        LispType::List(list) => {
            if list.len()==0 {
                // TODO: deal with empty list
                // println!("{}","Empty list!".green());
                return LispType::Atom(Atomic::nil);
            }
            let result = eval(&list[0], env);
            match result {
                LispType::Atom(Atomic::Symbol(keyword)) => {
                    match &keyword[..] {
                        "if" => {
                            // TODO: can be expanded to
                            // 1.(if test then...)
                            // 2.(if test then else...)
                            if let LispType::Atom(Atomic::t) = eval(&list[1], env) {
                                return eval(&list[2], env);
                            } else {
                                return eval(&list[3], env);
                            }
                        },
                        "define" => {
                            if let LispType::Atom(Atomic::Symbol(symbol_name)) = &list[1] {
                                // to avoid borrowing twice
                                let value = eval(&list[2], env);
                                env.add_symbol(symbol_name, &value);
                                // why not working? env_symbol.insert(symbol_name, eval(&list[2], env_symbol, env_op));
                            } else {
                                println!("{}","Not a valid symbol name!".red());
                            }
                        },
                        "write" => {
                            match &list[1] {
                                LispType::Atom(Atomic::Symbol(s)) => {
                                    print!("{}", s);
                                }
                                _ => {}
                            }
                        }
                        "quote" => {
                            return list[1].clone();
                        },
                        "set!" => {
                            if let LispType::Atom(Atomic::Symbol(symbol_name)) = &list[1] {
                                let value = eval(&list[2], env);
                                env.set_symbol(symbol_name, &value);
                            } else {
                                println!("{}","Not a valid symbol name!".red());
                            }
                        },
                        "lambda" => {
                            let mut param_list: Vec<String> = Vec::new();
                            if let LispType::List(_list) = &list[1] {
                                for param in _list{
                                    if let LispType::Atom(Atomic::Symbol(sym)) = param {
                                        param_list.push(sym.clone());
                                    } else {
                                        println!("{}:{:?}", "Invalid parameters".red(), param);
                                        return LispType::Atom(Atomic::nil);
                                    }
                                }
                            } else {
                                println!("{}\n{}", "Syntax Error".red(),"Usage: (lambda (symbol...) expr)".green());
                                return LispType::Atom(Atomic::nil);
                            }
                            return LispType::Atom(Atomic::Proc(Box::new(Proc::new(param_list, list[2].clone(), env.clone()))));
                        }
                        _ => {
                            println!("{}","Undefined keywords!".red());
                        }
                    }
                }
                LispType::Atom(Atomic::Fun(f)) => {
                    return f(list[1..].into_iter().map(|subexpr| eval(subexpr, env)).collect());
                },
                LispType::Atom(Atomic::Proc(mut uf)) => {
                    return uf.run(list[1..].into_iter().map(|subexpr| eval(subexpr, env)).collect(), env.clone());
                }
                _ => {
                    return result.clone();
                }
            }
        }
    }
    LispType::Atom(Atomic::nil)
}

pub fn init_env() ->Env{
    let mut env = Env::new(None);
    env.add_symbol(&String::from("PI"), &lisp_atom!(std::f64::consts::PI, Float));
    env.add_symbol(&String::from("if"), &lisp_atom!(String::from("if"), Symbol));
    env.add_symbol(&String::from("define"), &lisp_atom!(String::from("define"), Symbol));
    env.add_symbol(&String::from("quote"), &lisp_atom!(String::from("quote"), Symbol));
    env.add_symbol(&String::from("set!"), &lisp_atom!(String::from("set!"), Symbol));
    env.add_symbol(&String::from("lambda"), &lisp_atom!(String::from("lambda"), Symbol));
    env.add_symbol(&String::from("write"),&lisp_atom!(String::from("write"), Symbol));
    env.add_symbol(&String::from("+"), &lisp_atom!(add, Fun));
    env.add_symbol(&String::from("-"), &lisp_atom!(minus, Fun));
    env.add_symbol(&String::from("*"), &lisp_atom!(mul, Fun));
    env.add_symbol(&String::from("/"), &lisp_atom!(div, Fun));
    env.add_symbol(&String::from(">"), &lisp_atom!(gt, Fun));
    env.add_symbol(&String::from(">="), &lisp_atom!(ge, Fun));
    env.add_symbol(&String::from("<"), &lisp_atom!(lt, Fun));
    env.add_symbol(&String::from("<="), &lisp_atom!(le, Fun));
    env.add_symbol(&String::from("="), &lisp_atom!(eq, Fun));
    env.add_symbol(&String::from("/="), &lisp_atom!(neq, Fun));
    env.add_symbol(&String::from("begin"), &lisp_atom!(begin, Fun));
    env.add_symbol(&String::from("max"), &lisp_atom!(max, Fun));
    env.add_symbol(&String::from("min"), &lisp_atom!(min, Fun));
    env.add_symbol(&String::from("abs"), &lisp_atom!(abs, Fun));
    env.add_symbol(&String::from("append"), &lisp_atom!(append, Fun));
    env.add_symbol(&String::from("cons"), &lisp_atom!(cons, Fun));
    env.add_symbol(&String::from("car"), &lisp_atom!(car, Fun));
    env.add_symbol(&String::from("cdr"), &lisp_atom!(cdr, Fun));
    env.add_symbol(&String::from("apply"), &lisp_atom!(apply, Fun));
    env.add_symbol(&String::from("map"), &lisp_atom!(map, Fun));
    env.add_symbol(&String::from("list"), &lisp_atom!(list_, Fun));
    env
}

/*********************************/
/**** Below are env functions ****/
/*********************************/

macro_rules! get_atom_value {
    ($atom:expr, $ret_type:ty) => {match $atom {
        LispType::Atom(Atomic::Number(n)) => n as $ret_type,
        LispType::Atom(Atomic::Float(fp)) => fp as $ret_type,
        _ => {
            println!("{}", "Operands should be of type i32 or f64".red());
            -1 as $ret_type
        }
    }};
}

pub fn add(args: Vec<LispType>) ->LispType {
    // TODO: recursive
    let mut res_n = 0;
    let mut res_fp = 0.0;
    let mut flag = false;
    for atom in args.iter(){
        match atom {
            LispType::Atom(Atomic::Number(n)) => { res_n+=n; },
            LispType::Atom(Atomic::Float(fp)) => { res_fp+=fp; flag=true; },
            _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
        }
    }
    if flag {
        return lisp_atom!(res_fp+(res_n as f64), Float);
    }
    lisp_atom!(res_n, Number)
}

pub fn minus(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return if args.len()>1 {
                if let LispType::Atom(Atomic::Float(_fp)) = args[1] {
                    lisp_atom!(n as f64 - get_atom_value!(args[1],f64), Float)
                } else {
                    lisp_atom!(n - get_atom_value!(args[1], i32), Number)
                }
            } else {
                lisp_atom!(-get_atom_value!(args[0],i32), Number)
            }
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return if args.len()>1 {
                lisp_atom!(fp - get_atom_value!(args[1],f64), Float)
            } else {
                lisp_atom!(-get_atom_value!(args[0],f64), Float)
            }
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn mul(args: Vec<LispType>) ->LispType {
    let mut res_n = 1;
    let mut res_fp = 1.0;
    let mut flag = false;
    for atom in args.iter(){
        match atom {
            LispType::Atom(Atomic::Number(n)) => { res_n*=n; },
            LispType::Atom(Atomic::Float(fp)) => { res_fp*=fp; flag=true; },
            _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
        }
    }
    if flag {
        return lisp_atom!(res_fp*(res_n as f64), Float);
    }
    lisp_atom!(res_n, Number)
}

pub fn div(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return lisp_atom!(n as f64 / get_atom_value!(args[1], f64), Float);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(fp / get_atom_value!(args[1],f64), Float);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn gt(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return if ((n as f64) - get_atom_value!(args[1],f64))>std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return if (fp - get_atom_value!(args[1],f64))>std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn lt(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return if ((n as f64) - get_atom_value!(args[1],f64)) < -std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return if (fp - get_atom_value!(args[1],f64)) < -std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn ge(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return if ((n as f64) - get_atom_value!(args[1],f64))>= -std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return if (fp - get_atom_value!(args[1],f64))>= -std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn le(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return if ((n as f64) - get_atom_value!(args[1],f64))<=std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return if (fp - get_atom_value!(args[1],f64))<=std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn eq(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return if ((n as f64) - get_atom_value!(args[1],f64)).abs()<=std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return if (fp - get_atom_value!(args[1],f64)).abs()<=std::f64::EPSILON {
                LispType::Atom(Atomic::t)
            } else {
                LispType::Atom(Atomic::nil)
            }
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn neq(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return if ((n as f64) - get_atom_value!(args[1],f64)).abs()<=std::f64::EPSILON {
                LispType::Atom(Atomic::nil)
            } else {
                LispType::Atom(Atomic::t)
            }
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return if (fp - get_atom_value!(args[1],f64)).abs()<=std::f64::EPSILON {
                LispType::Atom(Atomic::nil)
            } else {
                LispType::Atom(Atomic::t)
            }
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn begin(args: Vec<LispType>) ->LispType {
    return args.last().unwrap().clone();
}

pub fn _max(arg0: &LispType, arg1: &LispType) -> LispType {
    match (&arg0, &arg1) {
        (LispType::Atom(Atomic::Number(n1)), LispType::Atom(Atomic::Number(n2))) => {
            return lisp_atom!(*n1.max(n2), Number);
        },
        (LispType::Atom(Atomic::Number(n1)), LispType::Atom(Atomic::Float(n2))) => {
            return lisp_atom!(n2.max(*n1 as f64), Float);
        }
        (LispType::Atom(Atomic::Float(n1)), LispType::Atom(Atomic::Number(n2))) => {
            return lisp_atom!(n1.max(*n2 as f64), Float);
        },
        (LispType::Atom(Atomic::Float(n1)), LispType::Atom(Atomic::Float(n2))) => {
            return lisp_atom!(n1.max(*n2), Float);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn max(args: Vec<LispType>) ->LispType {
    assert!(args.len()>1);
    let mut res = args[0].clone();
    for i in 1..args.len() {
        res = _max(&res, &args[i]);
    }
    res.clone()
}

pub fn _min(arg0: &LispType, arg1: &LispType) -> LispType {
    match (&arg0, &arg1) {
        (LispType::Atom(Atomic::Number(n1)), LispType::Atom(Atomic::Number(n2))) => {
            return lisp_atom!(*n1.min(n2), Number);
        },
        (LispType::Atom(Atomic::Number(n1)), LispType::Atom(Atomic::Float(n2))) => {
            return lisp_atom!(n2.min(*n1 as f64), Float);
        }
        (LispType::Atom(Atomic::Float(n1)), LispType::Atom(Atomic::Number(n2))) => {
            return lisp_atom!(n1.min(*n2 as f64), Float);
        },
        (LispType::Atom(Atomic::Float(n1)), LispType::Atom(Atomic::Float(n2))) => {
            return lisp_atom!(n1.min(*n2), Float);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn min(args: Vec<LispType>) ->LispType {
    assert!(args.len()>1);
    let mut res = args[0].clone();
    for i in 1..args.len() {
        res = _min(&res, &args[i]);
    }
    res.clone()
}

pub fn abs(args: Vec<LispType>) ->LispType{
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {return lisp_atom!(n.abs(), Number); },
        LispType::Atom(Atomic::Float(n)) => { return lisp_atom!(n.abs(), Float); },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    LispType::Atom(Atomic::nil)
}

/*
(map Op '<list> '<list> ... '<list>)
 */
pub fn map(args: Vec<LispType>) ->LispType{
    let mut res_list: Vec<LispType> = Vec::new();
    match &args[0] {
        LispType::Atom(Atomic::Fun(_f)) => {
            match &args[1] {
                LispType::List(list) => {
                    let n = list.len();
                    let mut v: Vec<Vec<LispType>> = Vec::new();
                    for i in 0..n {
                        v.push(vec![list[i].clone()]);
                    }
                    for i in 2..args.len() {
                        match &args[i] {
                            LispType::List(_list) => {
                                if _list.len() != n {
                                    println!("{}", "Arguments except for the 1st should be list of the same size".red());
                                    return LispType::Atom(Atomic::nil);
                                }
                                for (j, elem) in _list.iter().enumerate() {
                                    v[j].push(elem.clone());
                                }
                            }
                            _ => {
                                println!("{}", "Arguments except for the 1st should be of type list".red());
                                return LispType::Atom(Atomic::nil);
                            }
                        }
                    }
                    for i in 0..n {
                        res_list.push(_f(v[i].clone()));
                    }
                    return LispType::List(res_list);
                },
                _ => { println!("{}", "Arguments except for the 1st should be of type list".red()); }
            }
        },
        _ => { println!("{}", "The first argument of 'map' should be a function".red()); }
    }
    LispType::Atom(Atomic::nil)
}

/*
(apply Op '<List>)
(apply Op <Atom> <Atom> ... '<List>)
 */
pub fn apply(args: Vec<LispType>) ->LispType{
    match &args[0] {
        LispType::Atom(Atomic::Fun(_f)) => {
            let mut expanded_args = (&args[1..args.len()-1]).to_owned();
            match args.last().unwrap() {
                LispType::List(list) => {
                    for elem in list {
                        expanded_args.push(elem.clone());
                    }
                    return _f(expanded_args);
                }
                _ => { println!("{}","The last argument of 'apply' should be a list".red()); }
            }
        }
        _ => { println!("{}","The first argument of 'apply' should be a function".red()); }
    }
    LispType::Atom(Atomic::nil)
}

pub fn car(args: Vec<LispType>) ->LispType{
    match &args[0] {
        LispType::List(_list) => {
            return _list[0].clone();
        }
        _ => {}
    }
    LispType::Atom(Atomic::nil)
}

pub fn cdr(args: Vec<LispType>) ->LispType{
    match &args[0] {
        LispType::List(_list) => {
            return LispType::List(_list[1..].to_owned());
        }
        _ => {}
    }
    LispType::Atom(Atomic::nil)
}

pub fn append(args: Vec<LispType>) ->LispType{
    let mut n_list: Vec<LispType> = Vec::new();
    for arg in args.iter() {
        if let LispType::List(_list) = arg {
            n_list.append(&mut _list.clone());
        } else {
            println!("{}","Arguments should be of type List".red());
        }
    }
    LispType::List(n_list)
}


pub fn cons(args: Vec<LispType>) ->LispType{
    // TODO empty list
    match &args[1] {
        LispType::Atom(atom) => {
            return if let Atomic::nil = atom {
                LispType::List(vec![args[0].clone()])
            } else {
                LispType::List(vec![args[0].clone(),args[1].clone()])
            }
        },
        LispType::List(list) => {
            let mut retv = vec![args[0].clone()];
            for e in list.iter(){
                retv.push(e.clone());
            }
            return LispType::List(retv);
        }
    }
}

pub fn list_(args: Vec<LispType>) ->LispType{
    return LispType::List(args);
}

/*********************************/
/******* Below are tests *********/
/*********************************/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut env = Box::new(init_env());
        assert_eq!(Eval("(+ 1 2 3 4 5)", &mut env), LispType::Atom(Atomic::Number(15)));
        assert!((get_atom_value!(Eval("(+ 1.1 2.2 3.3 4.4 5.5)", &mut env),f64)-16.5).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_minus() {
        let mut env = Box::new(init_env());
        assert_eq!(Eval("(- 2 1)", &mut env), lisp_atom!(1, Number));
        assert!((get_atom_value!(Eval("(- 2.2 1.3)", &mut env),f64)-0.9).abs()<=std::f64::EPSILON);
        assert_eq!(Eval("(- 2)", &mut env), lisp_atom!(-2, Number));
        assert!((get_atom_value!(Eval("(- 2.2)", &mut env),f64)+2.2).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_mul() {
	let mut env = Box::new(init_env());
	assert_eq!(get_atom_value!(Eval("(* 1234 2021)", &mut env),i32),1234*2021);
	assert!((get_atom_value!(Eval("(* 3.1415 -20.21)", &mut env),f64)+3.1415*20.21).abs()<=std::f64::EPSILON);
	assert!((get_atom_value!(Eval("(* 3.14 12)", &mut env),f64)-3.14*12.0).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_div() {
	let mut env = Box::new(init_env());
	assert_eq!(get_atom_value!(Eval("(/ 1234 2021)", &mut env),f64),1234.0/2021.0);
	assert!((get_atom_value!(Eval("(/ 3.1415 -20.21)", &mut env),f64)+3.1415/20.21).abs()<=std::f64::EPSILON);
	assert!((get_atom_value!(Eval("(/ 3.14 12)", &mut env),f64)-3.14/12.0).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_cmp() {
	let mut env = Box::new(init_env());
        assert_eq!(Eval("(> 3.123 453.123)",&mut env),LispType::Atom(Atomic::nil));
        assert_eq!(Eval("(>= 3.123 3.123)",&mut env),LispType::Atom(Atomic::t));
        assert_eq!(Eval("(< -31 -30)", &mut env), LispType::Atom(Atomic::t));
        assert_eq!(Eval("(<= -3.21 -33.9)", &mut env), LispType::Atom(Atomic::nil));
        assert_eq!(Eval("(/= 3.14 3.14)", &mut env), LispType::Atom(Atomic::nil));
    }

    #[test]
    fn test_minmax() {
        let mut env = Box::new(init_env());
        assert_eq!(get_atom_value!(Eval("(max 1 2)", &mut env),i32),2);
        assert_eq!(get_atom_value!(Eval("(min -3 -1)",&mut env),i32),-3);
        assert!((get_atom_value!(Eval("(max (min -3.14 2.8) (max 11.2 19.8))",&mut env),f64)-19.8).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_begin() {
        let mut env = Box::new(init_env());
        assert!((get_atom_value!(Eval("(begin (+ 3 1) (- 3.14 5) (max 10.1 9.8))", &mut env),f64)-10.1).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_abs() {
        let mut env = Box::new(init_env());
        assert_eq!(get_atom_value!(Eval("(abs -11)",&mut env),i32),11);
        assert!((get_atom_value!(Eval("(abs -3.1415)",&mut env),f64)-3.1415).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_map() {
        let mut env = Box::new(init_env());
        assert_eq!(format!("{}",Eval("(map + '(1 2 3) '(4 5 6) '(7 8 9))",&mut env)),String::from("(12 15 18)"));
    }

    #[test]
    fn test_apply_and_define() {
        let mut env = Box::new(init_env());
        Eval("(define E 2.7)", &mut env);
        assert!((get_atom_value!(Eval("(apply + -1 E PI '(10 20.5))",&mut env), f64)-35.34159265358979).abs()<=std::f64::EPSILON);
    }

    #[test]
    fn test_car_cdr_if_list() {
        let mut env = Box::new(init_env());
        Eval("(define E 2.7)", &mut env);
        assert!((get_atom_value!(Eval("(if (> 10.1 10.0) (car (list E PI)) (cdr (list E PI (* E PI))))",&mut env), f64)-2.7).abs()<=std::f64::EPSILON);
        assert_eq!(format!("{}", Eval("(if (< 10.1 10.0) (car (list E PI)) (cdr (list E PI (* E PI))))",&mut env)), "(3.141592653589793 8.482300164692441)");
    }

    #[test]
    fn test_append() {
        let mut env = Box::new(init_env());
        Eval("(define r 10)", &mut env);
        assert_eq!(format!("{}", Eval("(append '(2.1 2.2) (list r (* r r)))",&mut env)), "(2.1 2.2 10 100)");
    }

    #[test]
    fn test_cons() {
        let mut env = Box::new(init_env());
        Eval("(define E 2.7)", &mut env);
        assert_eq!(format!("{}", Eval("(cons 3.14 E)",&mut env)), "(3.14 2.7)");
        assert_eq!(format!("{}", Eval("(cons 3.14 '(10 1.2))",&mut env)), "(3.14 10 1.2)");
        assert_eq!(format!("{}", Eval("(cons 3.14 (cons E (cons 1 nil)))",&mut env)), "(3.14 2.7 1)");
    }
}
