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
    SemiColon,
}

fn main() {
    let reserved_words = vec![
        String::from("let"),
        String::from("fn"),
        String::from("true"),
        String::from("false"),
        String::from("if"),
        String::from("else"),
        String::from("return"),
    ];
    let reserved_word_tokens = vec![
        Token::Let,
        Token::Fn,
        Token::True,
        Token::False,
        Token::If,
        Token::Else,
        Token::Return,
    ];

    let reserved_word_map: HashMap<_, _> = reserved_words
        .iter()
        .zip(reserved_word_tokens.iter())
        .collect();
    let input = String::from("fn let xxx  = if true else return false ;");
    print!("{:?}", tokenize(input, reserved_word_map));
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

fn tokenize(str: String, reserved_word_map: HashMap<&String, &Token>) -> Vec<Token> {
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
        if is_letter(&c) {
            let (letter, pss) = get_letter(&str, p);
            if let Some(&reserved_token) = reserved_word_map.get(&letter) {
                tokens.push(reserved_token.clone())
            } else {
                tokens.push(Token::Var(letter.clone().parse().unwrap()));
            }
            p = pss;
        } else if c == String::from(" ") {
            p += 1
        } else if c == String::from("=") {
            tokens.push(Token::Equal);
            p += 1;
        } else if c == String::from(";") {
            tokens.push(Token::SemiColon);
            p += 1;
        } else if is_num(&c) {
            let (n, pos) = get_num(&str, p);
            p = pos;
            let n: i32 = n.parse().unwrap();
            tokens.push(Token::Int(n));
        } else {
            p += 1;
        }
    }
}
