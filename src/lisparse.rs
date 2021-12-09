use std::collections::HashMap;
use regex::Regex;
use colored::Colorize;
// RE2 Number(R"(^-?\+?0|[1-9]\d*$)");
// RE2 Float(R"(^-?\d+\.\d+$)");

macro_rules! lisp_atom {
    ($var:expr, $atom_type:ident) => {
        LispType::Atom(Atomic::$atom_type($var))
    };
}

#[derive(Debug, Clone)]
pub enum Atomic {
    Number(i32),
    Float(f64),
    Symbol(String),  // TODO: refactor to &str
    Fun(fn(Vec<LispType>)->LispType)
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

pub fn is_func(s: &String, env_op: &HashMap<String, fn(Vec<LispType>)->LispType>) ->bool {
    return env_op.contains_key(s);
}

pub fn is_symbol(s: &String, env_symbol: &HashMap<String, LispType>) ->bool {
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

#[derive(Debug, Clone)]
pub struct Proc {
    params: Vec<String>,
    expr: LispType
}

impl Proc {
    pub fn new(params: Vec<String>, expr:LispType) ->Proc {
        return Proc{params, expr};
    }
    // pub fn run(args: Vec<LispType>) ->LispType {
    //
    // }
}

#[derive(Debug,Clone)]
pub struct Env {
    local_env_symbol: HashMap<String, LispType>,
    local_env_op: HashMap<String, fn(Vec<LispType>)->LispType>,
    parent_env: Option<Box<Env>>
}

impl Env {
    fn lookup_symbol(&self, s: &String) ->Result<LispType, &'static str>{
        return if self.local_env_symbol.contains_key(s) {
            Ok(self.local_env_symbol[s].clone())
        } else {
            match &(self.parent_env) {
                None => Err("Keyword Error: Undefined symbol"),
                Some(ptr) => ptr.lookup_symbol(s)
            }
        }
    }
    fn lookup_fun(&self, s: &String) ->Result<LispType, &'static str>{
        return if self.local_env_op.contains_key(s) {
            Ok(lisp_atom!(self.local_env_op[s], Fun))
        } else {
            match &(self.parent_env) {
                None => Err("Keyword Error: Undefined symbol"),
                Some(ptr) => ptr.lookup_fun(s)
            }
        }
    }
}

pub fn eval(expr: &LispType, env_symbol: &mut HashMap<String, LispType>,
                            env_op: &mut HashMap<String, fn(Vec<LispType>)->LispType>) -> LispType {
    match expr {
        LispType::Atom(atom) => {
            match atom {
                Atomic::Symbol(sym) => {
                    if is_func(sym, env_op) {
                        return lisp_atom!(env_op[sym],Fun);
                    } else if is_symbol(sym, env_symbol) {
                        return env_symbol[sym].clone();
                    } else {
                        println!("{} {}",sym.green(),"is not defined!\n".red());
                        return lisp_atom!(-1, Number);
                    }
                },
                _ => {  return LispType::Atom(atom.clone()); }
            };
        }
        LispType::List(list) => {
            if list.len()==0 {
                // TODO: deal with empty list
                println!("{}","Empty list!".green());
                return lisp_atom!(-1,Number);
            }
            let result = eval(&list[0], env_symbol, env_op);
            match result {
                LispType::Atom(Atomic::Symbol(op_name)) => {
                    match &op_name[..] {
                        "if" => {
                            // TODO: can be expanded to
                            // 1.(if test then...)
                            // 2.(if test then else...)
                            if let LispType::Atom(Atomic::Number(1)) = eval(&list[1], env_symbol, env_op) {
                                return eval(&list[2], env_symbol, env_op);
                            } else {
                                return eval(&list[3], env_symbol, env_op);
                            }
                        },
                        "define" => {
                            if let LispType::Atom(Atomic::Symbol(symbol_name)) = &list[1] {
                                // to avoid borrowing twice
                                let value = eval(&list[2], env_symbol, env_op);
                                use std::collections::hash_map::Entry;
                                match env_symbol.entry(symbol_name.clone()) {
                                    Entry::Occupied(entry) => {
                                        println!("{}","You can't define a symbol twice".red());
                                    },
                                    Entry::Vacant(entry) => {
                                        entry.insert(value);
                                    }
                                }
                                // why not working? env_symbol.insert(symbol_name, eval(&list[2], env_symbol, env_op));
                            } else {
                                println!("{}","Not a valid symbol name!".red());
                            }
                        },
                        "quote" => {
                            return list[1].clone();
                        },
                        "set!" => {
                            if let LispType::Atom(Atomic::Symbol(symbol_name)) = &list[1] {
                                let value = eval(&list[2], env_symbol, env_op);
                                use std::collections::hash_map::Entry;
                                match env_symbol.entry(symbol_name.clone()) {
                                    Entry::Occupied(mut entry) => {
                                        entry.insert(value);
                                    },
                                    Entry::Vacant(entry) => {
                                        println!("{}","Assigning to a undefined symbol is not allowed".red());
                                    }
                                }
                            } else {
                                println!("{}","Not a valid symbol name!".red());
                            }
                        },
                        "lambda" => {

                        }
                        _ => {
                            println!("{}","Undefined keywords!".red());
                        }
                    }
                },
                LispType::Atom(Atomic::Fun(f)) => {
                    return f(list[1..].into_iter().map(|subexpr| eval(subexpr, env_symbol, env_op)).collect());
                },
                _ => {
                    return result.clone();
                }
            }
        }
    }
    LispType::List(Vec::new())  // empty list, aka nil
}

pub fn init_env_symbol() ->HashMap<String, LispType> {
    let mut env_symbol: HashMap<String, LispType> = HashMap::new();
    env_symbol.insert(String::from("PI"), LispType::Atom(Atomic::Float(std::f64::consts::PI)));
    env_symbol.insert(String::from("if"), LispType::Atom(Atomic::Symbol(String::from("if"))));
    env_symbol.insert(String::from("define"), LispType::Atom(Atomic::Symbol(String::from("define"))));
    env_symbol
}

pub fn init_env_op() ->HashMap<String, fn(Vec<LispType>)->LispType> {
    let mut env_op: HashMap<String, fn(Vec<LispType>)->LispType> = HashMap::new();
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
    env_op.insert(String::from("max"), max);
    env_op.insert(String::from("min"), min);
    env_op.insert(String::from("abs"), abs);
    env_op.insert(String::from("append"), append);
    env_op.insert(String::from("cons"), cons);
    env_op.insert(String::from("car"), car);
    env_op.insert(String::from("cdr"), cdr);
    env_op.insert(String::from("apply"), apply);
    env_op.insert(String::from("map"), map);
    env_op
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
            return lisp_atom!(n - get_atom_value!(args[1],i32), Number);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(fp - get_atom_value!(args[1],f64), Float);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    lisp_atom!(-1, Number)
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
    lisp_atom!(-1, Number)
}

pub fn gt(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return lisp_atom!(i32::from((n as f64) > get_atom_value!(args[1],f64)), Number);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(i32::from(fp > get_atom_value!(args[1],f64)), Number);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    lisp_atom!(0, Number)
}

pub fn lt(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return lisp_atom!(i32::from((n as f64) < get_atom_value!(args[1],f64)), Number);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(i32::from(fp < get_atom_value!(args[1],f64)), Number);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    lisp_atom!(0, Number)
}

pub fn ge(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return lisp_atom!(i32::from((n as f64) >= get_atom_value!(args[1],f64)), Number);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(i32::from(fp >= get_atom_value!(args[1],f64)), Number);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    lisp_atom!(0, Number)
}

pub fn le(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return lisp_atom!(i32::from((n as f64) <= get_atom_value!(args[1],f64)), Number);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(i32::from(fp <= get_atom_value!(args[1],f64)), Number);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    lisp_atom!(0, Number)
}

pub fn eq(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return lisp_atom!(i32::from((n as f64) == get_atom_value!(args[1],f64)), Number);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(i32::from(fp == get_atom_value!(args[1],f64)), Number);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    lisp_atom!(0, Number)
}

pub fn neq(args: Vec<LispType>) ->LispType {
    match args[0] {
        LispType::Atom(Atomic::Number(n)) => {
            return lisp_atom!(i32::from((n as f64) != get_atom_value!(args[1],f64)), Number);
        },
        LispType::Atom(Atomic::Float(fp)) => {
            return lisp_atom!(i32::from(fp != get_atom_value!(args[1],f64)), Number);
        },
        _ => { println!("{}", "Operands should be of type i32 or f64".red()); }
    }
    lisp_atom!(0, Number)
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
    lisp_atom!(-1, Number)
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
    lisp_atom!(-1, Number)
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
    lisp_atom!(0, Number)
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
                                    return lisp_atom!(-1, Number);
                                }
                                for (j, elem) in _list.iter().enumerate() {
                                    v[j].push(elem.clone());
                                }
                            }
                            _ => {
                                println!("{}", "Arguments except for the 1st should be of type list".red());
                                return lisp_atom!(-1,Number);
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
    lisp_atom!(-1, Number)
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
    lisp_atom!(-1, Number)
}

pub fn car(args: Vec<LispType>) ->LispType{
    match &args[0] {
        LispType::List(_list) => {
            return _list[0].clone();
        }
        _ => {}
    }
    lisp_atom!(-1, Number)
}

pub fn cdr(args: Vec<LispType>) ->LispType{
    match &args[0] {
        LispType::List(_list) => {
            return LispType::List(_list[1..].to_owned());
        }
        _ => {}
    }
    lisp_atom!(-1, Number)
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
            return LispType::List(vec![args[0].clone(),args[1].clone()]);
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