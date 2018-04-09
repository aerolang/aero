//! Handles breaking up text input into tokens which can be parsed by
//! the parser.

use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq, Clone, Debug)]
pub enum Symbol {
    Plus,
    Hyph,
    Star,
    Slash,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    SnakeCase(String),
    CamelCase(String),
    SpecialCase(String),
    Int32(i32),
    Op(Symbol),
    Space,
    Newline,
    End
}

/// Build a vector of tokens off a string input.
pub fn tokenize(string: String) -> Result<Vec<Token>, String> {
    // Getting a peekable iterator so we can look ahead.
    let mut iter = string.chars().peekable();
    let mut result = vec![];

    loop {
        match iter.peek() {
            // If we have more characters, we'll grab the next token
            // and add it to the list.
            Some(&next_ch) => match next_token(next_ch, &mut iter) {
                Ok(token) => result.push(token),
                Err(message) => return Err(message)
            },
            // Otherwise, we'll finish off the token list with the
            // end token.
            None => {
                result.push(Token::End);
                break;
            }
        }
    }

    Ok(result)
}

type TokenResult = Result<Token, String>;

// Get the next token from the iterator given the next character.
fn next_token(next_char: char, iter: &mut Peekable<Chars>) -> TokenResult {
    match next_char {
        ' ' | '\t' | '\n' | '\r' => take_whitespace(iter),
        'a' ... 'z' | 'A' ... 'Z' | '_' => take_ident(iter),
        '0' ... '9' => take_number_lit(iter),
        '+' => take_token(iter, Token::Op(Symbol::Plus)),
        '-' => take_token(iter, Token::Op(Symbol::Hyph)),
        '*' => take_token(iter, Token::Op(Symbol::Star)),
        '/' => take_token(iter, Token::Op(Symbol::Slash)),
        _ => Err(format!("unexpected character \"{}\"", next_char))
    }
}

// Get the next identifier.
fn take_ident(iter: &mut Peekable<Chars>) -> TokenResult {
    let mut ident_vec = vec![];

    let mut has_lower = false;
    let mut has_upper = false;
    let mut has_underscore = false;

    let starts_upper = match *iter.peek().unwrap() {
        'A' ... 'Z' => true,
        _ => false
    };

    while let Some(&ch) = iter.peek() {
        match ch {
            'a' ... 'z' => has_lower = true,
            'A' ... 'Z' => has_upper = true,
            '_' => has_underscore = true,
            '0' ... '9' => (),
            _ => break
        }

        iter.next().unwrap();
        ident_vec.push(ch);
    }

    let ident = ident_vec.into_iter().collect();

    if has_upper && has_underscore && !has_lower {
        Ok(Token::SpecialCase(ident))
    } else if starts_upper && !has_underscore {
        Ok(Token::CamelCase(ident))
    } else if !has_upper {
        Ok(Token::SnakeCase(ident))
    } else {
        Err(format!("invalid identifier \"{}\"", ident))
    }
}

// Get the whitespace token (Space or Newline) from the whitespace.
fn take_whitespace(iter: &mut Peekable<Chars>) -> TokenResult {
    let whitespace = take_while(iter, |ch| match ch {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false
    });

    // This is a newline token if it contains either a line feed or
    // carriage return. Otherwise, it is just a space token.
    let has_newline = whitespace.contains(&'\n') || whitespace.contains(&'\r');

    if has_newline {
        Ok(Token::Newline)
    } else {
        Ok(Token::Space)
    }
}

// Get a number literal.
fn take_number_lit(iter: &mut Peekable<Chars>) -> TokenResult {
    let number: String = take_while(iter, |ch| ch.is_numeric())
        .into_iter()
        .collect();

    let parsed: i32 = number.parse().unwrap();

    Ok(Token::Int32(parsed))
}

// Take the next character off the iterator and return the token.
fn take_token(iter: &mut Peekable<Chars>, token: Token) -> TokenResult {
    iter.next().unwrap();
    Ok(token)
}

// Keep taking off the iterator while a closure returns true.
fn take_while<F>(iter: &mut Peekable<Chars>, cond: F) -> Vec<char>
        where F: Fn(char) -> bool {
    let mut result = vec![];

    while let Some(&ch) = iter.peek() {
        if cond(ch) {
            iter.next().unwrap();
            result.push(ch);
        } else {
            break;
        }
    }

    result
}

#[test]
fn addition_test() {
    let test_str = "1 + 2";

    assert_eq!(tokenize(String::from(test_str)).unwrap(), vec![
        Token::Int32(1),
        Token::Space,
        Token::Op(Symbol::Plus),
        Token::Space,
        Token::Int32(2),
        Token::End
    ]);
}

#[test]
fn ident_test() {
    let test_str = "test Test T test1 test_1 _ __TEST__";

    assert_eq!(tokenize(String::from(test_str)).unwrap(), vec![
        Token::SnakeCase(String::from("test")),
        Token::Space,
        Token::CamelCase(String::from("Test")),
        Token::Space,
        Token::CamelCase(String::from("T")),
        Token::Space,
        Token::SnakeCase(String::from("test1")),
        Token::Space,
        Token::SnakeCase(String::from("test_1")),
        Token::Space,
        Token::SnakeCase(String::from("_")),
        Token::Space,
        Token::SpecialCase(String::from("__TEST__")),
        Token::End
    ]);
}

#[test]
fn whitespace_test() {
    let test_str = "\r\n  12/ 34\n";

    assert_eq!(tokenize(String::from(test_str)).unwrap(), vec![
        Token::Newline,
        Token::Int32(12),
        Token::Op(Symbol::Slash),
        Token::Space,
        Token::Int32(34),
        Token::Newline,
        Token::End
    ]);
}
