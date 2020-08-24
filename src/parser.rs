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

type Parameters = Vec<Exp>;
type Arguments = Vec<Exp>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Exp {
    Int(i32),
    Var(String),
    Bool(bool),
    If {
        cond_exp: Box<Exp>,
        then_stmts: Vec<Statement>,
        else_stmts: Vec<Statement>,
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
    Func {
        params: Parameters,
        body: Vec<Statement>,
    },
    FuncCall {
        funcName: Box<Exp>,
        args: Arguments,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Let { id: Exp, value: Exp },
    ExpStmt { exp: Exp },
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

pub fn parse_exp(tokens: &[Token], p: Precedence) -> (Exp, &[Token]) {
    let (left, rest) = match tokens {
        [first, rest @ ..] => match first {
            Token::Int(n) => (Exp::Int(*n), rest),
            Token::Var(v) => (Exp::Var(v.clone()), rest),
            Token::True => (Exp::Bool(true), rest),
            Token::False => (Exp::Bool(false), rest),
            Token::Bang | Token::Minus => parse_prefix_exp(tokens),
            Token::LParen => parse_grouped_exp(tokens),
            Token::If => parse_if(tokens),
            Token::Fn => parse_func(tokens),
            _ => panic!("error prefix exp"),
        },
        _ => panic!("error prefix exp"),
    };
    parse_exp_core(rest, left, p)
}

fn parse_exp_core(tokens: &[Token], left: Exp, p: Precedence) -> (Exp, &[Token]) {
    match tokens {
        [Token::SemiColon, rest @ ..] => (left, rest),
        [op, _rest @ ..] => {
            let precedence = get_precedence(op);
            if p < precedence {
                let (result, rest) = match op {
                    Token::Plus
                    | Token::Minus
                    | Token::Asterisk
                    | Token::Slash
                    | Token::Equal
                    | Token::NotEqual
                    | Token::Lt
                    | Token::Gt => parse_infix_exp(tokens, left),
                    _ => {
                        return (left, tokens);
                    }
                };
                parse_exp_core(rest, result, p)
            } else {
                (left, tokens)
            }
        }

        _ => (left, tokens),
    }
}

fn parse_grouped_exp(tokens: &[Token]) -> (Exp, &[Token]) {
    let (exp, rest) = match tokens {
        [Token::LParen, rest @ ..] => parse_exp(rest, Precedence::LOWEST),
        _ => panic!("error"),
    };

    match rest {
        [Token::RParen, rest @ ..] => (exp, rest),
        _ => {
            println!("{:?}", rest);
            panic!("error: かっこが閉じてない ");
        }
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
            let (right, rest) = parse_exp(rest, p);
            let p = Exp::InfixExp {
                left: Box::new(left),
                op: op,
                right: Box::new(right),
            };
            (p, rest)
        }
        _ => {
            panic!("error");
        }
    }
}

fn parse_let(tokens: &[Token]) -> (Statement, &[Token]) {
    match tokens {
        [Token::Let, Token::Var(s), Token::Assign, rest @ ..] => {
            let (exp, rest) = parse_exp(rest, Precedence::LOWEST);
            match rest {
                [Token::SemiColon, rest @ ..] => (
                    Statement::Let {
                        id: Exp::Var(s.clone()),
                        value: exp,
                    },
                    rest,
                ),
                _ => (
                    Statement::Let {
                        id: Exp::Var(s.clone()),
                        value: exp,
                    },
                    rest,
                ),
            }
        }
        _ => panic!("let error"),
    }
}

fn parse_exp_statement(tokens: &[Token]) -> (Statement, &[Token]) {
    let (exp, rest) = parse_exp(tokens, Precedence::LOWEST);
    (Statement::ExpStmt { exp: exp }, rest)
}

fn parse_statement(tokens: &[Token]) -> (Statement, &[Token]) {
    match tokens {
        [Token::Let, _rest @ ..] => parse_let(tokens),
        _ => parse_exp_statement(tokens),
    }
}

fn parse_block_statements(tokens: &[Token], mut acm: Vec<Statement>) -> (Vec<Statement>, &[Token]) {
    match tokens {
        [Token::RBrace, rest @ ..] => (acm, rest),
        [Token::LBrace, rest @ ..] => {
            let (stmt, rest) = parse_statement(rest);
            acm.push(stmt);
            parse_block_statements(rest, acm)
        }
        _ => {
            let (stmt, rest) = parse_statement(tokens);
            acm.push(stmt);
            parse_block_statements(rest, acm)
        }
    }
}

fn parse_func(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::Fn, rest @ ..] => {
            let empty_vec: Vec<Exp> = vec![];
            let (params, rest) = parse_params(rest, empty_vec);
            let empty_vec: Vec<Statement> = vec![];
            let (stmts, rest) = parse_block_statements(rest, empty_vec);
            (
                Exp::Func {
                    params: params,
                    body: stmts,
                },
                rest,
            )
        }
        _ => {
            println!("{:?}", tokens);
            panic!("error");
        }
    }
}

fn parse_func_call(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::Var(v), rest @ ..] => {
            let empty_vec: Vec<Exp> = vec![];
            let (args, rest) = parse_args(rest, empty_vec);
            (
                Exp::FuncCall {
                    funcName: Box::new(Exp::Var(v.clone())),
                    args: args,
                },
                rest,
            )
        }
        _ => {
            println!("{:?}", tokens);
            panic!("error");
        }
    }
}

fn parse_params(tokens: &[Token], mut acm: Vec<Exp>) -> (Parameters, &[Token]) {
    match tokens {
        [Token::RParen, rest @ ..] => (acm, rest),
        [Token::LParen, rest @ ..] => {
            let (exp, rest) = parse_exp(rest, Precedence::LOWEST);
            acm.push(exp);
            parse_params(rest, acm)
        }
        [Token::Comma, rest @ ..] => {
            let (exp, rest) = parse_exp(rest, Precedence::LOWEST);
            acm.push(exp);
            parse_params(rest, acm)
        }
        _ => {
            panic!("error");
        }
    }
}

fn parse_args(tokens: &[Token], mut acm: Vec<Exp>) -> (Arguments, &[Token]) {
    match tokens {
        [Token::RParen, rest @ ..] => (acm, rest),
        [Token::LParen, rest @ ..] => {
            let (exp, rest) = parse_exp(rest, Precedence::LOWEST);
            acm.push(exp);
            parse_args(rest, acm)
        }
        [Token::Comma, rest @ ..] => {
            let (exp, rest) = parse_exp(rest, Precedence::LOWEST);
            acm.push(exp);
            parse_args(rest, acm)
        }
        _ => {
            panic!("error");
        }
    }
}

fn parse_if(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::If, rest @ ..] => {
            let (cond_exp, rest) = parse_exp(rest, Precedence::LOWEST);
            let emp_vec: Vec<Statement> = Vec::new();
            let (then_stmts, rest) = parse_block_statements(rest, emp_vec);
            match rest {
                [Token::Else, rest @ ..] => {
                    let emp_vec: Vec<Statement> = Vec::new();
                    let (else_stmts, rest) = parse_block_statements(rest, emp_vec);
                    (
                        Exp::If {
                            cond_exp: Box::new(cond_exp),
                            then_stmts: then_stmts,
                            else_stmts: else_stmts,
                        },
                        rest,
                    )
                }
                _ => {
                    let emp_vec: Vec<Statement> = Vec::new();
                    (
                        Exp::If {
                            cond_exp: Box::new(cond_exp),
                            then_stmts: then_stmts,
                            else_stmts: emp_vec,
                        },
                        rest,
                    )
                }
            }
        }
        _ => {
            panic!("if error");
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

    let input = "(5 - (4 + 2));";
    let tokens = start_to_tokenize(input);
    let (result, _) = parse_exp(tokens.as_slice(), Precedence::LOWEST);
    assert_eq!(
        result,
        Exp::InfixExp {
            left: Box::new(Exp::Int(5)),
            op: Operator::Minus,
            right: Box::new(Exp::InfixExp {
                left: Box::new(Exp::Int(4)),
                op: Operator::Plus,
                right: Box::new(Exp::Int(2)),
            }),
        }
    );
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
    1 + if (true) {
        1;
    } else {
        2;
    }
    ";
    let tokens = start_to_tokenize(input);
    let (exp, _) = parse_exp(&tokens, Precedence::LOWEST);
    let then_stmts = vec![Statement::ExpStmt { exp: Exp::Int(1) }];
    let else_stmts = vec![Statement::ExpStmt { exp: Exp::Int(2) }];
    assert_eq!(
        exp,
        Exp::InfixExp {
            left: Box::new(Exp::Int(1)),
            op: Operator::Plus,
            right: Box::new(Exp::If {
                cond_exp: Box::new(Exp::Bool(true)),
                then_stmts: then_stmts,
                else_stmts: else_stmts,
            })
        }
    );

    let input = "
   if (true) {
       let x = 2;
        1;
    }
    ";
    let tokens = start_to_tokenize(input);
    let (exp, _) = parse_exp(&tokens, Precedence::LOWEST);
    let then_stmts = vec![
        Statement::Let {
            id: Exp::Var("x".to_string()),
            value: Exp::Int(2),
        },
        Statement::ExpStmt { exp: Exp::Int(1) },
    ];
    let else_stmts: Vec<Statement> = vec![];
    assert_eq!(
        exp,
        Exp::If {
            cond_exp: Box::new(Exp::Bool(true)),
            then_stmts: then_stmts,
            else_stmts: else_stmts,
        }
    );
}
#[test]
fn test_precedence() {
    assert_eq!(Precedence::CALL > Precedence::SUM, true);
    assert_eq!(Precedence::SUM > Precedence::LOWEST, true);
}

#[test]
fn test_parse_stmt() {
    let input = "
    func(x, y) {
        let x = 2;
    }
    ";
    let tokens = start_to_tokenize(input);
    let (func, _) = parse_func(tokens.as_slice());
    assert_eq!(
        func,
        Exp::Func {
            params: vec![Exp::Var("x".to_string()), Exp::Var("y".to_string())],
            body: vec![Statement::Let {
                id: Exp::Var("x".to_string()),
                value: Exp::Int(2),
            }],
        }
    );
}

#[test]
fn test_parse_func_call() {
    let input = "
    add(x+1, y);
    ";
    let tokens = start_to_tokenize(input);
    let (func_call, _) = parse_func_call(tokens.as_slice());
    assert_eq!(
        func_call,
        Exp::FuncCall {
            funcName: Box::new(Exp::Var("add".to_string())),
            args: vec![
                Exp::InfixExp {
                    left: Box::new(Exp::Var("x".to_string())),
                    op: Operator::Plus,
                    right: Box::new(Exp::Int(1))
                },
                Exp::Var("y".to_string())
            ]
        }
    );
}
