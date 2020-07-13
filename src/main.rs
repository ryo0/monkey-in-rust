#[derive(Debug)]
enum Token {
    Let,
    Mut,
    Equal,
    Var(String),
    Int(i32),
    SemiColon,
}

fn main() {
    let input = String::from("let mut a z  xxx ;");
    print!("{}", is_letter(";"));
    print!("{}", is_letter("l"));
    print!("{:?}", tokenize(input));
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

fn get_letter(str: &String, position: i32)-> (String, i32) {
    let mut p = position;
    loop {
        let c = get_char(str, p);
        if is_letter(&c) {
            p += 1;
        } else {
            return (str[position as usize..p as usize].to_string(), p)
        }
    }
}

fn tokenize(str: String)-> Vec<Token> {
    let mut p = 0;
    let mut tokens: Vec<Token> = vec!();
    loop {
        let c = get_char(&str, p);
        if p >= str.len() as i32 {
            return tokens
        }
        if c.is_empty() {
            return tokens
        }
        if is_letter(&c) {
            let (letter, pss) = get_letter(&str, p);
            tokens.push(Token::Var(letter));
            p = pss;
        } else {
            p += 1;
        }

    }
}
