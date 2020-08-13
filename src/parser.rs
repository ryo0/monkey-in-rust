use crate::lexer::start_to_tokenize;
use crate::lexer::Token;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Exp {
    Int(i32),
    Var(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Let { id: Exp, value: Exp },
}

fn parse_exp(tokens: &[Token]) -> Exp {
    match tokens {
        [Token::Int(n), _rest @ ..] => Exp::Int(*n),
        _ => panic!("error"),
    }
}

fn parse_let(tokens: &[Token]) -> Statement {
    match tokens {
        [Token::Let, Token::Var(s), Token::Assign, rest @ ..] => Statement::Let {
            id: Exp::Var(s.clone()),
            value: parse_exp(rest),
        },
        _ => panic!("let error"),
    }
}

#[test]
fn test_parse_let() {
    let input = "let x = 2;";
    let tokens = start_to_tokenize(input);
    let result = parse_let(&tokens);
    assert_eq!(
        result,
        Statement::Let {
            id: Exp::Var("x".to_string()),
            value: Exp::Int(2)
        }
    )
}
