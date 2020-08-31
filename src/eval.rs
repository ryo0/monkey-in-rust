use crate::lexer::start_to_tokenize;
use crate::parser::{parse_exp, start_to_parse, Exp, Operator, Precedence, Program, Statement};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Env {
    env: HashMap<String, Value>,
    next: Option<Rc<RefCell<Env>>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Value {
    Int {
        val: i32,
    },
    Bool {
        val: bool,
    },
    Null,
    Func {
        params: Vec<Exp>,
        body: Vec<Statement>,
        env: Rc<RefCell<Env>>,
    },
    StringVal {
        val: String,
    },
    Array {
        array: Vec<Value>,
    },
}

fn get_value(env: &Env, key: String) -> Value {
    let v = env.env.get(&key);
    match v {
        Some(v) => v.clone(),
        None => match env.next.clone() {
            Some(env_next) => get_value(&env_next.as_ref().borrow(), key),
            None => {
                panic!("no value in env error");
            }
        },
    }
}

fn eval_program(program: Program, env: &mut Rc<RefCell<Env>>) -> Value {
    let mut value = Value::Null;
    for stmt in program {
        match stmt {
            Statement::Let { id, value } => {
                eval_let(Statement::Let { id, value }, env);
            }
            Statement::ExpStmt { exp } => {
                let evaled_value = eval_exp(exp, env);
                value = evaled_value;
            }
            Statement::Return { exp } => {
                let evaled_value = eval_exp(exp, env);
                return evaled_value;
            }
            _ => {
                panic!("未対応");
            }
        }
    }
    value
}

fn eval_let(letStmt: Statement, env: &mut Rc<RefCell<Env>>) -> Value {
    match letStmt {
        Statement::Let {
            id: Exp::Var(n),
            value,
        } => {
            let evaled_value = eval_exp(value, env);
            env.borrow_mut().env.insert(n, evaled_value.clone());
            evaled_value
        }
        _ => {
            panic!("error");
        }
    }
}

fn eval_exp(exp: Exp, env: &mut Rc<RefCell<Env>>) -> Value {
    match exp {
        Exp::InfixExp { left, op, right } => {
            let left = eval_exp(*left, env);
            let right = eval_exp(*right, env);
            match (left, right) {
                (Value::Int { val: l }, Value::Int { val: r }) => match op {
                    Operator::Plus => Value::Int { val: l + r },
                    Operator::Minus => Value::Int { val: l - r },
                    Operator::Asterisk => Value::Int { val: l * r },
                    Operator::Slash => Value::Int { val: l / r },
                    Operator::Less => Value::Bool { val: l < r },
                    Operator::Greater => Value::Bool { val: l > r },
                    Operator::Equal => Value::Bool { val: l == r },
                    _ => panic!("error 未対応"),
                },
                (Value::Bool { val: l }, Value::Bool { val: r }) => match op {
                    Operator::Equal => Value::Bool { val: l == r },
                    Operator::NotEqual => Value::Bool { val: l != r },
                    _ => panic!("error BoolのOperator"),
                },
                (left, right) => Value::Bool { val: left == right },
                _ => {
                    panic!("両辺が同じ型でない中置式");
                }
            }
        }
        Exp::PrefixExp { op, right } => {
            let right = eval_exp(*right, env);
            match right {
                Value::Int { val: r } => match op {
                    Operator::Minus => Value::Int { val: -r },
                    _ => {
                        panic!("未対応");
                    }
                },
                Value::Bool { val: r } => match op {
                    Operator::Bang => Value::Bool { val: !r },
                    _ => {
                        panic!("未対応");
                    }
                },
                _ => {
                    panic!("null");
                }
            }
        }
        Exp::Int(n) => Value::Int { val: n },
        Exp::Bool(b) => Value::Bool { val: b },
        Exp::Null => Value::Null,
        Exp::StringVal(s) => Value::StringVal { val: s },
        Exp::Var(n) => get_value(&env.borrow(), n),
        Exp::If {
            cond_exp,
            then_stmts,
            else_stmts,
        } => {
            let evaled_cond = eval_exp(*cond_exp, env);
            match evaled_cond {
                Value::Bool { val } => {
                    if val {
                        eval_program(then_stmts, env)
                    } else {
                        eval_program(else_stmts, env)
                    }
                }
                _ => {
                    panic!("条件式がboolでない");
                }
            }
        }
        Exp::Func { params, body } => Value::Func {
            params: params,
            body: body,
            env: Rc::clone(&env),
        },
        Exp::FuncCall { func_name, args } => {
            let evaled_func = eval_exp(*func_name, env);
            let mut evaled_args: Vec<Value> = vec![];
            for arg in args {
                let evaled_arg = eval_exp(arg, env);
                evaled_args.push(evaled_arg);
            }
            let mut new_env_hash: HashMap<String, Value> = HashMap::new();
            match evaled_func {
                Value::Func { params, body, env } => {
                    println!("evaled_args.len(): {:?}", evaled_args.len());
                    for i in 0..evaled_args.len() {
                        match params[i].clone() {
                            Exp::Var(n) => {
                                let evaled_arg = evaled_args[i].clone();
                                new_env_hash.insert(n, evaled_arg);
                            }
                            _ => {
                                panic!("error");
                            }
                        }
                    }
                    let mut new_env = Rc::new(RefCell::new(Env {
                        env: new_env_hash,
                        next: Some(env),
                    }));
                    // println!("env is {:?}", new_env);
                    eval_program(body, &mut new_env)
                }
                _ => {
                    println!("evaled_func{:?}", evaled_func);
                    panic!("error");
                }
            }
        }
        Exp::Array { vec } => {
            let mut evaled_vec: Vec<Value> = vec![];
            for exp in vec {
                let evaled_exp = eval_exp(exp, env);
                evaled_vec.push(evaled_exp);
            }
            Value::Array { array: evaled_vec }
        }
        Exp::IndexExp { left, index } => {
            let array = eval_exp(*left, env);
            let index = eval_exp(*index, env);
            match (array, index) {
                (Value::Array { array }, Value::Int { val }) => {
                    if val as usize >= array.len() || val < 0 {
                        Value::Null
                    } else {
                        match array.get(val as usize) {
                            Some(val) => val.clone(),
                            None => panic!("error"),
                        }
                    }
                }
                _ => {
                    panic!("array index error");
                }
            }
        }
        _ => {
            println!("{:?}", exp);
            panic!("error 未対応");
        }
    }
}
#[test]
fn test_get_eval() {
    let mut e1: HashMap<String, Value> = HashMap::new();
    let mut e2: HashMap<String, Value> = HashMap::new();
    e1.insert("x".to_string(), Value::Int { val: 1 });
    e2.insert("y".to_string(), Value::Int { val: 3 });
    let next_env = Env {
        env: e2,
        next: None,
    };
    let env: Env = Env {
        env: e1,
        next: Some(Rc::new(RefCell::new(next_env))),
    };
    assert_eq!(get_value(&env, "x".to_string()), Value::Int { val: 1 });
    assert_eq!(get_value(&env, "y".to_string()), Value::Int { val: 3 });
}

#[test]
fn test_eval_exp() {
    let mut emp = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let input = "-1 - 2 * -3;";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 5 });

    let mut emp = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let input = "(5 - (4 + 2));";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: -1 });

    let mut emp = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let input = "(5 - (4 + 2) + 1);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 0 });

    let mut emp = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let input = "(5 - (4 + 2) * (2 - 1) * 7 + 2);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: -35 });

    let mut emp = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let input = "2*(3+4)-5+4;";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 13 });

    let mut emp = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let input = "2*(3+(4*2+12*35)-54)*(2+4);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 4524 });

    let mut emp = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let input = "6*(53 - 4) * (4 + 5 *( 5 - (4 + 2) * (2 - 1) * 7 + 2) * 2 + 5 * 7);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: -91434 });
}

#[test]
fn test_if_exp() {
    let input = "if(true) {
        1 + 10;
    } else {
        2 + 12;
    }";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 11 });
}

#[test]
fn test_func() {
    let input = "
    func(x) {
        return 3 + 1;
        true;
    };
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    let s = Exp::Var("x".to_string());
    assert_eq!(
        result,
        Value::Func {
            params: vec![s],
            body: vec![
                Statement::Return {
                    exp: Exp::InfixExp {
                        left: Box::new(Exp::Int(3)),
                        op: Operator::Plus,
                        right: Box::new(Exp::Int(1)),
                    }
                },
                Statement::ExpStmt {
                    exp: Exp::Bool(true),
                }
            ],
            env: env,
        }
    );
}

#[test]
fn test_func_call() {
    let input = "
    let f = func(x) {
        return 3 + 1;
        true;
    };
    f(2);
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 4 });

    let input = "
    let f = func(x) {
        return x + 1;
        true;
    };
    f(2);
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 3 });

    let input = "
    let y = 10;
    let f = func(x) {
        return x + y;
        true;
    };
    f(2);
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 12 });

    let input = "
    let y = 10;
    let f = func(x) {
        let y = 100;
        return x + y;
        true;
    };
    f(2);
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 102 });
}

#[test]
fn test_rec_func_call() {
    // 環境をprintln!しようとすると、循環参照のため無限にループが回ってしまう模様
    let input = "
    let f = func(x, acm) {
        if(x == 0) {
            return acm;
        } else {
            return f(x-1, acm+1);
        }
    };
    f(5, 0);
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 5 });

    // 環境をprintln!しようとすると、循環参照のため無限にループが回ってしまう模様
    let input = "
        let len = func(array, n) {
            if(array[n] == null) {
                return n;
            } else {
                return len(array, n+1);
            }
        };
        len([1, 2, 3, 4 , 5], 0);
        ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 5 });
}
#[test]
fn test_rec_closure_func_call() {
    // 環境をprintln!しようとすると、循環参照のため無限にループが回ってしまう模様
    let input = "
    let closure = func() {
        let y = 100;
        let f = func(x, acm) {
            if(x == 0) {
                return acm + y;
            } else {
                return f(x-1, acm+1);
            }
        };
        return f;
    };
    let y = 1;
    let g = closure();
    g(5, 0);
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 105 });
}

#[test]
fn test_return_exp() {
    let input = "
    if(true) {
        let x = 2;
        return x + 1;
        3;
    } else {
        return false;
    };
    ";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 3 });
}

#[test]
fn test_eval_let() {
    let input = "let x = 2;
    x + 1";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 3 });

    let input = "
    let x = 10;
    let y = 20;
    x - y";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: -10 });

    let input = "
    let x = 10;
    let x = 20;
    x - 10";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    println!("{:?}", p);
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 10 });
}

#[test]
fn test_eval_array() {
    let input = "
    let double = func(x) {return x * 2;};
    [1, double(2), 3*3, 4-3];";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(
        result,
        Value::Array {
            array: vec![
                Value::Int { val: 1 },
                Value::Int { val: 4 },
                Value::Int { val: 9 },
                Value::Int { val: 1 }
            ]
        }
    );
}

#[test]
fn test_eval_indexing() {
    let input = "
    let double = func(x) {return x * 2;};
    [1, double(2), 3*3, 4-3][0+1];";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 4 });

    let input = "
    let double = func(x) {return x * 2;};
    [1, double(2), 3*3, 4-3][0+1 - 3];";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Null);

    let input = "
    let arrayFunc = func() {return [1, 2, 3];} ;
    let double = func(x) {return x * 2;};
    arrayFunc()[double(1)];";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 3 });

    let input = "
    let array = [1, 2, 3] ;
    let double = func(x) {return x * 2;};
    array[double(1)];";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Rc::new(RefCell::new(Env {
        env: HashMap::new(),
        next: None,
    }));
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 3 });
}
