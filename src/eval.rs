use crate::lexer::start_to_tokenize;
use crate::parser::{parse_exp, start_to_parse, Exp, Operator, Precedence, Program, Statement};
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Env {
    env: HashMap<String, Value>,
    next: Option<Box<Env>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Value {
    Int { val: i32 },
    Bool { val: bool },
}

fn get_value(env: &Env, key: String) -> Value {
    println!("{:?}", env);
    let v = env.env.get(&key);
    match v {
        Some(v) => v.clone(),
        None => match env.next.clone() {
            Some(env_next) => get_value(&env_next, key),
            None => {
                panic!("error");
            }
        },
    }
}

fn eval_program(program: Program, env: &mut Env) -> Value {
    let mut value = Value::Int { val: 0 };
    for stmt in program {
        match stmt {
            Statement::Let { id, value } => {
                eval_let(Statement::Let { id, value }, env);
            }
            Statement::ExpStmt { exp } => {
                let evaled_value = eval_exp(exp, env);
                value = evaled_value;
            }
            _ => {
                panic!("未対応");
            }
        }
    }
    value
}

fn eval_let(letStmt: Statement, env: &mut Env) -> Value {
    match letStmt {
        Statement::Let {
            id: Exp::Var(n),
            value,
        } => {
            let evaled_value = eval_exp(value, env);
            env.env.insert(n, evaled_value.clone());
            evaled_value
        }
        _ => {
            panic!("error");
        }
    }
}

fn eval_exp(exp: Exp, env: &mut Env) -> Value {
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
                    _ => panic!("error 未対応"),
                },
                (Value::Bool { val: l }, Value::Bool { val: r }) => match op {
                    Operator::Equal => Value::Bool { val: l == r },
                    Operator::NotEqual => Value::Bool { val: l != r },
                    _ => panic!("error BoolのOperator"),
                },
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
            }
        }
        Exp::Int(n) => Value::Int { val: n },
        Exp::Var(n) => get_value(&env, n),
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
        _ => panic!("error 未対応"),
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
        next: Some(Box::new(next_env)),
    };
    assert_eq!(get_value(&env, "x".to_string()), Value::Int { val: 1 });
    assert_eq!(get_value(&env, "y".to_string()), Value::Int { val: 3 });
}

#[test]
fn test_eval_exp() {
    let mut emp: Env = Env {
        env: HashMap::new(),
        next: None,
    };
    let input = "-1 - 2 * -3;";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 5 });

    let mut emp: Env = Env {
        env: HashMap::new(),
        next: None,
    };
    let input = "(5 - (4 + 2));";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: -1 });

    let mut emp: Env = Env {
        env: HashMap::new(),
        next: None,
    };
    let input = "(5 - (4 + 2) + 1);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 0 });

    let mut emp: Env = Env {
        env: HashMap::new(),
        next: None,
    };
    let input = "(5 - (4 + 2) * (2 - 1) * 7 + 2);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: -35 });

    let mut emp: Env = Env {
        env: HashMap::new(),
        next: None,
    };
    let input = "2*(3+4)-5+4;";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 13 });

    let mut emp: Env = Env {
        env: HashMap::new(),
        next: None,
    };
    let input = "2*(3+(4*2+12*35)-54)*(2+4);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: 4524 });

    let mut emp: Env = Env {
        env: HashMap::new(),
        next: None,
    };
    let input = "6*(53 - 4) * (4 + 5 *( 5 - (4 + 2) * (2 - 1) * 7 + 2) * 2 + 5 * 7);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result, &mut emp);
    assert_eq!(result, Value::Int { val: -91434 });
}

#[test]
fn test_eval_let() {
    let input = "let x = 2;
    x + 1";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    let mut env = Env {
        env: HashMap::new(),
        next: None,
    };
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 3 });

    let input = "
    let x = 10;
    let y = 20;
    x - y";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    println!("{:?}", p);
    let mut env = Env {
        env: HashMap::new(),
        next: None,
    };
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: -10 });

    let input = "
    let x = 10;
    let x = 20;
    x - 10";
    let tokens = start_to_tokenize(input);
    let p = start_to_parse(tokens.as_slice());
    println!("{:?}", p);
    let mut env = Env {
        env: HashMap::new(),
        next: None,
    };
    let result = eval_program(p, &mut env);
    assert_eq!(result, Value::Int { val: 10 });
}
