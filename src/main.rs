use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Token {
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

fn main() {
    let two_words_symbol_first = vec![String::from("="), String::from("!")];
    let two_words_symbol_second = vec![
        (String::from("="), Token::Equal),
        (String::from("="), Token::NotEqual),
    ];
    let two_words_symbol_map: HashMap<_, _> = two_words_symbol_first
        .iter()
        .zip(two_words_symbol_second.iter())
        .collect();
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
    let input = String::from("fn let xxx  = if true else return false ;");
    print!("{:?}", tokenize(input, tokens_map, two_words_symbol_map));
}

fn get_char(str: &String, position: i32) -> String {
    match str.chars().nth(position as usize) {
        Some(x) => x.to_string(),
        None => String::from(""),
    }
}

fn is_letter(s: &str) -> bool {
    if let Some(c) = s.chars().nth(0) {
        if c.is_alphabetic() {
            true
        } else if c == '_' {
            true
        } else {
            false
        }
    } else {
        false
    }
}

fn is_num(s: &str) -> bool {
    if let Some(c) = s.chars().nth(0) {
        if c.is_numeric() {
            true
        } else {
            false
        }
    } else {
        false
    }
}

fn get_letter(str: &String, position: i32) -> (String, i32) {
    let mut p = position;
    loop {
        let c = get_char(str, p);
        if is_letter(&c) {
            p += 1;
        } else {
            return (str[position as usize..p as usize].to_string(), p);
        }
    }
}

fn get_num(str: &String, position: i32) -> (String, i32) {
    let mut p = position;
    loop {
        let c = get_char(str, p);
        if is_num(&c) {
            p += 1;
        } else {
            return (str[position as usize..p as usize].to_string(), p);
        }
    }
}

fn tokenize(
    str: String,
    token_map: HashMap<&String, &Token>,
    two_words_map: HashMap<&String, &(String, Token)>,
) -> Vec<Token> {
    let mut p = 0;
    let mut tokens: Vec<Token> = vec![];
    loop {
        let c = get_char(&str, p);
        if p >= str.len() as i32 {
            return tokens;
        }
        if c.is_empty() {
            return tokens;
        }
        if c == String::from(" ") || c == String::from("\n") {
            p += 1;
        }
        if is_letter(&c) {
            let (letter, pss) = get_letter(&str, p);
            if let Some(&reserved_token) = token_map.get(&letter) {
                tokens.push(reserved_token.clone())
            } else {
                tokens.push(Token::Var(letter));
            }
            p = pss;
            if is_num(&c) {
                let (n, pos) = get_num(&str, p);
                p = pos;
                let n: i32 = n.parse().unwrap();
                tokens.push(Token::Int(n));
            } else {
                p += 1;
            }
        } else if is_num(&c) {
            let (n, pos) = get_num(&str, p);
            p = pos;
            let n: i32 = n.parse().unwrap();
            tokens.push(Token::Int(n));
        } else if c == String::from("\n") {
            p += 1
        } else if c == String::from(" ") {
            p += 1
        } else if let Some(&(got_next_c, token)) = two_words_map.get(&c) {
            let next_c = if p + 1 < str.len() as i32 {
                get_char(&str, p + 1)
            } else {
                panic!("error",);
            };
            if &next_c == got_next_c {
                tokens.push(token.clone());
                p += 2
            } else {
                panic!("error")
            }
        } else if let Some(&t) = token_map.get(&c) {
            tokens.push(t.clone());
            p += 1;
        } else {
            panic!("error");
        }
    }
}
