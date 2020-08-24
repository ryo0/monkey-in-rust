use crate::lexer::{start_to_tokenize, Token};
use crate::parser::{parse_exp, Exp, Operator, Precedence};

#[derive(Debug, Clone, Eq, PartialEq)]
enum Value {
    Int { val: i32 },
}

fn eval_exp(exp: Exp) -> Value {
    match exp {
        Exp::InfixExp { left, op, right } => {
            let left = eval_exp(*left);
            let right = eval_exp(*right);
            match (left, right) {
                (Value::Int { val: l }, Value::Int { val: r }) => match op {
                    Operator::Plus => Value::Int { val: l + r },
                    Operator::Minus => Value::Int { val: l - r },
                    Operator::Asterisk => Value::Int { val: l * r },
                    Operator::Slash => Value::Int { val: l / r },
                    _ => panic!("error 未対応"),
                },
            }
        }
        Exp::PrefixExp { op, right } => {
            let right = eval_exp(*right);
            match right {
                Value::Int { val: r } => match op {
                    Operator::Minus => Value::Int { val: -r },
                    _ => {
                        panic!("未対応");
                    }
                },
            }
        }
        Exp::Int(n) => Value::Int { val: n },
        _ => panic!("未対応"),
    }
}

#[test]
fn test_eval_exp() {
    let input = "-1 - 2 * -3;";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result);
    assert_eq!(result, Value::Int { val: 5 });

    let input = "6*(53 - 4) * (4 + 5 *( 5 - (4 + 2) * (2 - 1) * 7 + 2) * 2 + 5 * 7);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result);
    // assert_eq!(result, Value::Int { val: -91434 });

    let input = "(5 - (4 + 2) * (2 - 1) * 7 + 2);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    let result = eval_exp(result);
    assert_eq!(result, Value::Int { val: -35 });
}
