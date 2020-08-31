use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Token {
    Let,
    Fn,
    True,
    False,
    Null,
    If,
    Else,
    Return,
    Var(String),
    Int(i32),
    StringVal(String),
    Equal,
    NotEqual,
    SemiColon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Lt,
    Gt,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    DoubleQuote,
    Colon,
}

pub fn start_to_tokenize(input: &str) -> Vec<Token> {
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
        String::from("["),
        String::from("]"),
        String::from(":"),
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
        Token::LBracket,
        Token::RBracket,
        Token::Colon,
    ];

    let mut words = vec![
        String::from("let"),
        String::from("func"),
        String::from("true"),
        String::from("false"),
        String::from("null"),
        String::from("if"),
        String::from("else"),
        String::from("return"),
    ];

    let mut reserved_word_tokens = vec![
        Token::Let,
        Token::Fn,
        Token::True,
        Token::False,
        Token::Null,
        Token::If,
        Token::Else,
        Token::Return,
    ];

    words.append(&mut symbols);
    reserved_word_tokens.append(&mut symbol_tokens);

    let tokens_map: HashMap<_, _> = words.iter().zip(reserved_word_tokens.iter()).collect();

    let mut t = Vec::new();

    let result = tokenize(split_string(input).as_slice(), &mut t, tokens_map).1;
    result.clone()
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
        [first, rest @ ..] if first == &'"' => tokenize_string(rest, tokens, token_map),
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

fn tokenize_string<'a, 'b>(
    s: &'a [char],
    tokens: &'b mut Vec<Token>,
    token_map: HashMap<&String, &Token>,
) -> (&'a [char], &'b Vec<Token>) {
    let (letter, rest) = get_letter(s, String::from(""));
    tokens.push(Token::StringVal(letter));
    match rest {
        ['"', rest @ ..] => tokenize(rest, tokens, token_map),
        _ => {
            panic!("string quoteが閉じてない");
        }
    }
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

#[test]
fn test_tokenize() {
    let input = "
   :;
  let add = func(x, y) {
      let a = xx_ + y_y - z < w * a / b != k == d;
  };
  if (5 < 10) {
      return true;
  } else {
      return false;
  };
  \"let\";
  \"string\";
  [a, 1, true];";
    assert_eq!(
        start_to_tokenize(input),
        vec![
            Token::Colon,
            Token::SemiColon,
            Token::Let,
            Token::Var("add".to_string()),
            Token::Assign,
            Token::Fn,
            Token::LParen,
            Token::Var("x".to_string()),
            Token::Comma,
            Token::Var("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Let,
            Token::Var("a".to_string()),
            Token::Assign,
            Token::Var("xx_".to_string()),
            Token::Plus,
            Token::Var("y_y".to_string()),
            Token::Minus,
            Token::Var("z".to_string()),
            Token::Lt,
            Token::Var("w".to_string()),
            Token::Asterisk,
            Token::Var("a".to_string()),
            Token::Slash,
            Token::Var("b".to_string()),
            Token::NotEqual,
            Token::Var("k".to_string()),
            Token::Equal,
            Token::Var("d".to_string()),
            Token::SemiColon,
            Token::RBrace,
            Token::SemiColon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::SemiColon,
            Token::RBrace,
            Token::SemiColon,
            Token::StringVal("let".to_string()),
            Token::SemiColon,
            Token::StringVal("string".to_string()),
            Token::SemiColon,
            Token::LBracket,
            Token::Var("a".to_string()),
            Token::Comma,
            Token::Int(1),
            Token::Comma,
            Token::True,
            Token::RBracket,
            Token::SemiColon,
        ]
    )
}
