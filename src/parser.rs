use crate::lexer::start_to_tokenize;
use crate::lexer::Token;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Exp {
    Int(i32),
    Var(String),
    Bool(bool),
    If {
        cond_exp: Box<Exp>,
        then_exp: Box<Exp>,
        else_exp: Box<Option<Exp>>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Let { id: Exp, value: Exp },
}

fn parse_exp(tokens: &[Token]) -> (Exp, &[Token]) {
    let (exp, rest) = match tokens {
        [Token::LParen, rest @ ..] => parse_exp(rest),
        [Token::Int(n), rest @ ..] => (Exp::Int(*n), rest),
        [Token::True, rest @ ..] => (Exp::Bool(true), rest),
        [Token::False, rest @ ..] => (Exp::Bool(false), rest),
        _ => {
            println!("{:?}", tokens);
            panic!("error")
        }
    };
    match rest {
        [Token::RParen, Token::SemiColon, rest @ ..] => (exp, rest),
        [Token::SemiColon, rest @ ..] => (exp, rest),
        [Token::RParen, rest @ ..] => (exp, rest),
        _ => (exp, rest),
    }
}

fn parse_let(tokens: &[Token]) -> (Statement, &[Token]) {
    match tokens {
        [Token::Let, Token::Var(s), Token::Assign, rest @ ..] => {
            let (exp, rest) = parse_exp(rest);
            (
                Statement::Let {
                    id: Exp::Var(s.clone()),
                    value: exp,
                },
                rest,
            )
        }
        _ => panic!("let error"),
    }
}

fn parse_if(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::If, rest @ ..] => {
            let (cond_exp, rest) = parse_exp(rest);
            match rest {
                [Token::LBrace, rest @ ..] => {
                    let (then_exp, rest) = parse_exp(rest);
                    match rest {
                        [Token::RBrace, rest @ ..] => match rest {
                            [Token::Else, Token::LBrace, rest @ ..] => {
                                let (else_exp, rest) = parse_exp(rest);
                                let if_exp = Exp::If {
                                    cond_exp: Box::new(cond_exp),
                                    then_exp: Box::new(then_exp),
                                    else_exp: Box::new(Some(else_exp)),
                                };
                                match rest {
                                    [Token::RBrace, rest @ ..] => (if_exp, rest),
                                    _ => {
                                        println!("{:?}", rest);
                                        panic!("error0");
                                    }
                                }
                            }
                            _ => {
                                let if_exp = Exp::If {
                                    cond_exp: Box::new(cond_exp),
                                    then_exp: Box::new(then_exp),
                                    else_exp: Box::new(None),
                                };
                                match rest {
                                    [Token::RBrace, rest @ ..] => (if_exp, rest),
                                    _ => {
                                        println!("{:?}", rest);
                                        panic!("error0");
                                    }
                                }
                            }
                        },
                        _ => {
                            println!("{:?}", rest);
                            panic!("if error1");
                        }
                    }
                }
                _ => {
                    println!("{:?}", rest);
                    panic!("if error2");
                }
            }
        }
        _ => {
            println!("{:?}", tokens);
            panic!("if error3");
        }
    }
}

#[test]
fn test_parse_let() {
    let input = "let x = 2;";
    let tokens = start_to_tokenize(input);
    let result = parse_let(&tokens);
    assert_eq!(
        result,
        (
            Statement::Let {
                id: Exp::Var("x".to_string()),
                value: Exp::Int(2)
            },
            Vec::new().as_slice(),
        )
    )
}
#[test]
fn test_parse_if() {
    let input = "
    if (true) {
        1;
    } else {
        2;
    }
    ";
    let tokens = start_to_tokenize(input);
    let result = parse_if(&tokens);
    assert_eq!(
        result,
        (
            Exp::If {
                cond_exp: Box::new(Exp::Bool(true)),
                then_exp: Box::new(Exp::Int(1)),
                else_exp: Box::new(Some(Exp::Int(2))),
            },
            Vec::new().as_slice(),
        )
    )
}
