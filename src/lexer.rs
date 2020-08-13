use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Token {
    Let,
    Fn,
    True,
    False,
    If,
    Else,
    Return,
    Var(String),
    Int(i32),
    Equal,
    NotEqual,
    SemiColon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Lt,
    Gt,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    DoubleQuote,
}

pub fn start_to_tokenize(input: &str) {
    let mut symbols = vec![
        String::from("=="),
        String::from("!="),
        String::from(";"),
        String::from(","),
        String::from("("),
        String::from(")"),
        String::from("{"),
        String::from("}"),
        String::from("<"),
        String::from(">"),
        String::from("="),
        String::from("+"),
        String::from("-"),
        String::from("!"),
        String::from("*"),
        String::from("/"),
        String::from("\""),
    ];
    let mut symbol_tokens = vec![
        Token::Equal,
        Token::NotEqual,
        Token::SemiColon,
        Token::Comma,
        Token::LParen,
        Token::RParen,
        Token::LBrace,
        Token::RBrace,
        Token::Lt,
        Token::Gt,
        Token::Assign,
        Token::Plus,
        Token::Minus,
        Token::Bang,
        Token::Asterisk,
        Token::Slash,
        Token::DoubleQuote,
    ];

    let mut words = vec![
        String::from("let"),
        String::from("fn"),
        String::from("true"),
        String::from("false"),
        String::from("if"),
        String::from("else"),
        String::from("return"),
    ];

    let mut reserved_word_tokens = vec![
        Token::Let,
        Token::Fn,
        Token::True,
        Token::False,
        Token::If,
        Token::Else,
        Token::Return,
    ];

    words.append(&mut symbols);
    reserved_word_tokens.append(&mut symbol_tokens);

    let tokens_map: HashMap<_, _> = words.iter().zip(reserved_word_tokens.iter()).collect();
    print!(
        "{:?}",
        tokenize(split_string(input).as_slice(), &mut Vec::new(), tokens_map)
    );
}

fn split_string(s: &str) -> Vec<char> {
    s.chars().collect()
}

fn get_letter(s: &[char], acm: String) -> (String, &[char]) {
    match s {
        [first, rest @ ..] if first.is_alphabetic() || first == &'_' => {
            get_letter(rest, format!("{}{}", acm, first))
        }
        _ => (acm, s),
    }
}

fn get_num(s: &[char], acm: String) -> (String, &[char]) {
    match s {
        [first, rest @ ..] if first.is_numeric() => get_num(rest, format!("{}{}", acm, first)),
        _ => (acm, s),
    }
}

fn tokenize<'a, 'b>(
    s: &'a [char],
    tokens: &'b mut Vec<Token>,
    token_map: HashMap<&String, &Token>,
) -> (&'a [char], &'b Vec<Token>) {
    match s {
        [first, rest @ ..] if first == &' ' || first == &'\n' => tokenize(rest, tokens, token_map),
        [first, _rest @ ..] if first.is_alphabetic() => tokenize_letter(s, tokens, token_map),
        [first, _rest @ ..] if first.is_numeric() => tokenize_num(s, tokens, token_map),
        _ => tokenize_symbols(s, tokens, token_map),
    }
}

fn tokenize_num<'a, 'b>(
    s: &'a [char],
    tokens: &'b mut Vec<Token>,
    token_map: HashMap<&String, &Token>,
) -> (&'a [char], &'b Vec<Token>) {
    let (num, rest) = get_num(s, String::from(""));
    let token = num.parse::<i32>().unwrap();
    tokens.push(Token::Int(token));
    tokenize(rest, tokens, token_map)
}

fn tokenize_letter<'a, 'b>(
    s: &'a [char],
    tokens: &'b mut Vec<Token>,
    token_map: HashMap<&String, &Token>,
) -> (&'a [char], &'b Vec<Token>) {
    let (letter, rest) = get_letter(s, String::from(""));
    match token_map.get(&letter) {
        Some(token) => {
            tokens.push((*token).clone());
        }
        None => {
            tokens.push(Token::Var(letter));
        }
    };

    tokenize(rest, tokens, token_map)
}

fn tokenize_symbols<'a, 'b>(
    s: &'a [char],
    tokens: &'b mut Vec<Token>,
    token_map: HashMap<&String, &Token>,
) -> (&'a [char], &'b Vec<Token>) {
    match s {
        [first, second, rest @ ..] => match (first, second) {
            ('=', '=') | ('!', '=') => {
                let token = match token_map.get(&format!("{}{}", first, second)) {
                    Some(token) => *token,
                    None => panic!("token_map"),
                };
                tokens.push(token.clone());
                tokenize(rest, tokens, token_map)
            }
            _ => tokenize_symbol(s, tokens, token_map),
        },
        _ => tokenize_symbol(s, tokens, token_map),
    }
}

fn tokenize_symbol<'a, 'b>(
    s: &'a [char],
    tokens: &'b mut Vec<Token>,
    token_map: HashMap<&String, &Token>,
) -> (&'a [char], &'b Vec<Token>) {
    match s {
        [first, rest @ ..] => {
            let token = match token_map.get(&first.to_string()) {
                Some(token) => *token,
                None => panic!("token_map {} {:?}", first, rest),
            };
            tokens.push(token.clone());
            tokenize(rest, tokens, token_map)
        }
        _ => (s, tokens),
    }
}
