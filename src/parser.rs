use crate::lexer::{start_to_tokenize, Token};

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

fn start_to_parse(tokens: &[Token]) {}

fn parse_exp(tokens: &[Token], p: Precedence) -> (Exp, &[Token]) {
    let (left, rest) = match tokens {
        [first, rest @ ..] => match first {
            Token::Int(n) => (Exp::Int(*n), rest),
            Token::Var(v) => (Exp::Var(v.clone()), rest),
            Token::True => (Exp::Bool(true), rest),
            Token::False => (Exp::Bool(false), rest),
            Token::Bang | Token::Minus => parse_prefix_exp(tokens),
            Token::LParen => parse_grouped_exp(tokens),
            // Token::If => parse_if(tokens),
            _ => panic!("error prefix exp"),
        },
        _ => panic!("error prefix exp"),
    };
    println!("rest: {:?}", rest);
    parse_exp_core(rest, left, p)
}

fn parse_exp_core(tokens: &[Token], left: Exp, p: Precedence) -> (Exp, &[Token]) {
    match tokens {
        [Token::SemiColon, rest @ ..] => (left, rest),
        [first, rest @ ..] => {
            let precedence = get_precedence(first);
            if p < precedence {
                let (result, rest) = match first {
                    Token::Plus
                    | Token::Minus
                    | Token::Asterisk
                    | Token::Slash
                    | Token::Equal
                    | Token::NotEqual
                    | Token::Lt
                    | Token::Gt => parse_infix_exp(tokens, left),
                    Token::LParen => parse_grouped_exp(rest),
                    _ => (left, tokens),
                };
                parse_exp_core(rest, result, precedence)
            } else {
                (left, tokens)
            }
        }

        _ => (left, tokens),
    }
}

fn parse_grouped_exp(tokens: &[Token]) -> (Exp, &[Token]) {
    println!("{:?}", tokens);
    let (exp, rest) = match tokens {
        [LParen, rest @ ..] => parse_exp(rest, Precedence::LOWEST),
        _ => panic!("error"),
    };

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
            print!("{:?}", token);
            panic!("error");
        }
    }
}

fn parse_prefix_exp(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [first, rest @ ..] => {
            let op = convert_op_token(first);
            let (exp, rest) = parse_exp(rest, Precedence::PREFIX);
            let p = Exp::PrefixExp {
                op: op,
                right: Box::new(exp),
            };
            (p, rest)
        }
        _ => {
            panic!("error");
        }
    }
}

fn get_precedence(token: &Token) -> Precedence {
    match token {
        Token::Equal | Token::NotEqual => Precedence::EQUALS,
        Token::Lt | Token::Gt => Precedence::LESSGREATER,
        Token::Plus | Token::Minus => Precedence::SUM,
        Token::Slash | Token::Asterisk => Precedence::PRODUCT,
        Token::LParen => Precedence::CALL,
        _ => Precedence::LOWEST,
    }
}

fn parse_infix_exp(tokens: &[Token], left: Exp) -> (Exp, &[Token]) {
    match tokens {
        [first, rest @ ..] => {
            let op = convert_op_token(first);
            let p = get_precedence(first);
            let (exp, rest) = parse_exp(rest, p);
            let p = Exp::InfixExp {
                left: Box::new(left),
                op: op,
                right: Box::new(exp),
            };
            (p, rest)
        }
        _ => {
            panic!("error");
        }
    }
}

#[test]
fn test_parse_exp() {
    let input = "1 + 2;";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    assert_eq!(
        result,
        Exp::InfixExp {
            left: Box::new(Exp::Int(1)),
            op: Operator::Plus,
            right: Box::new(Exp::Int(2)),
        }
    );

    let input = "1 + (2 + 3);";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    assert_eq!(
        result,
        Exp::InfixExp {
            left: Box::new(Exp::Int(1)),
            op: Operator::Plus,
            right: Box::new(Exp::InfixExp {
                left: Box::new(Exp::Int(2)),
                op: Operator::Plus,
                right: Box::new(Exp::Int(3)),
            }),
        }
    );

    let input = "-1 - 2 * -3;";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    assert_eq!(
        result,
        Exp::InfixExp {
            left: Box::new(Exp::PrefixExp {
                op: Operator::Minus,
                right: Box::new(Exp::Int(1))
            }),
            op: Operator::Minus,
            right: Box::new(Exp::InfixExp {
                left: Box::new(Exp::Int(2)),
                op: Operator::Asterisk,
                right: Box::new(Exp::PrefixExp {
                    op: Operator::Minus,
                    right: Box::new(Exp::Int(3))
                }),
            }),
        }
    );
}

// fn parse_let(tokens: &[Token]) -> (Statement, &[Token]) {
//     match tokens {
//         [Token::Let, Token::Var(s), Token::Assign, rest @ ..] => {
//             let (exp, rest) = parse_exp(rest);
//             (
//                 Statement::Let {
//                     id: Exp::Var(s.clone()),
//                     value: exp,
//                 },
//                 rest,
//             )
//         }
//         _ => panic!("let error"),
//     }
// }

fn parse_if(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::If, rest @ ..] => {
            let (cond_exp, rest) = parse_exp(rest, Precedence::LOWEST);
            match rest {
                [Token::LBrace, rest @ ..] => {
                    let (then_exp, rest) = parse_exp(rest, Precedence::LOWEST);
                    match rest {
                        [Token::RBrace, rest @ ..] => match rest {
                            [Token::Else, Token::LBrace, rest @ ..] => {
                                let (else_exp, rest) = parse_exp(rest, Precedence::LOWEST);
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

// #[test]
// fn test_parse_let() {
//     let input = "let x = 2;";
//     let tokens = start_to_tokenize(input);
//     let result = parse_let(&tokens);
//     assert_eq!(
//         result,
//         (
//             Statement::Let {
//                 id: Exp::Var("x".to_string()),
//                 value: Exp::Int(2)
//             },
//             Vec::new().as_slice(),
//         )
//     )
// }
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
