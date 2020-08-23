use crate::lexer::Token;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Equal,
    NotEqual,
}

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
    PrefixExp {
        op: Operator,
        right: Box<Exp>,
    },
    InfixExp {
        left: Box<Exp>,
        op: Operator,
        right: Box<Exp>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Let { id: Exp, value: Exp },
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // my_cunction(x){}
    LBRACKET,    // []
}

type prefix_fns_type = Box<dyn Fn(&[Token]) -> (Exp, &[Token])>;
type infix_fns_type = Box<dyn Fn(&[Token], Precedence, Exp) -> (Exp, &[Token])>;

fn start_to_parse() {
    let mut prefix_parse_fns: HashMap<Token, prefix_fns_type> = HashMap::new();
    prefix_parse_fns.insert(Token::Bang, Box::new(parse_prefix_exp));
    prefix_parse_fns.insert(Token::Minus, Box::new(parse_prefix_exp));
    prefix_parse_fns.insert(Token::LParen, Box::new(parse_grouped_exp));
    let mut infix_parse_fns: HashMap<Token, infix_fns_type> = HashMap::new();
}

fn parse_grouped_exp(tokens: &[Token]) -> (Exp, &[Token]) {
    let (exp, rest) = parse_exp(tokens, Precedence::LOWEST);
    match rest {
        [Token::RParen, rest @ ..] => (exp, rest),
        _ => panic!("error: かっこが閉じてない"),
    }
}

fn convert_op_token(token: &Token) -> Operator {
    match token {
        Token::Plus => Operator::Plus,
        Token::Minus => Operator::Minus,
        Token::Asterisk => Operator::Asterisk,
        Token::Slash => Operator::Slash,
        Token::Equal => Operator::Equal,
        Token::NotEqual => Operator::NotEqual,
        Token::Bang => Operator::Bang,
        _ => {
            panic!("error");
        }
    }
}

fn parse_prefix_exp(tokens: &[Token]) -> (Exp::PrefixExp, &[Token]) {
    match tokens {
        [first ,rest @..] => {
            let op = convert_op_token(first);
            let (exp, rest) = parse_exp(rest, Precedence::PREFIX);
            let p = Exp::PrefixExp{op: op, right: exp};
            (exp, rest)
            }
            _ => {
                panic!("error");
            }
        }
    }
}

fn parse_exp(tokens: &[Token], p: Precedence) -> (Exp, &[Token]) {
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
