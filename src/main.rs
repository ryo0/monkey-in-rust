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
    let input = "
let add = fn(x, y) {
    let a = x + y - z < w * a / b != k == d;
};
if (5 < 10) {
    return true;
} else {
    return false;
}";
    print!("{:?}", tokenize(split_string(input).as_slice(), tokens_map));
}

fn split_string(s: &str) -> Vec<char> {
    s.chars().collect()
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

fn get_letter(s: &[char], acm: String) -> (String, &[char]) {
    match s {
        [first, rest @ ..] if first.is_alphabetic() || first == &'_' => {
            get_letter(rest, format!("{}{}", acm, first))
        }
        _ => (acm, s),
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

fn tokenize<'a, 'b>(
    s: &'a [char],
    tokens: &Vec<&Token>,
    token_map: HashMap<&String, &Token>,
) -> (&'a [char], &'b Vec<Token>) {
    match s {
        [first, rest @ ..] if first.is_alphabetic() => {
            let (letter, rest) = get_letter(s, String::from(""));
             let token = match token_map.get(&letter) {
                 Some(token) =>{ *token }
                 None =>{ panic!("token_map") }
             }
            tokens.push(token);
            tokenize(rest, tokens, token_map)
        }

        [first, rest @ ..] if first.is_numeric() => {
            
        }
    }
}
