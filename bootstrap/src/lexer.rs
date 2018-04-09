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
    Int32(i32),
    Operator(Symbol),
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
        '0' ... '9' => take_number_lit(iter),
        '+' => take_token(iter, Token::Operator(Symbol::Plus)),
        '-' => take_token(iter, Token::Operator(Symbol::Hyph)),
        '*' => take_token(iter, Token::Operator(Symbol::Star)),
        '/' => take_token(iter, Token::Operator(Symbol::Slash)),
        _ => return Err(format!("unexpected character \"{}\"", next_char))
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
    assert_eq!(tokenize(String::from("1 + 2")).unwrap(), vec![
        Token::Int32(1),
        Token::Space,
        Token::Operator(Symbol::Plus),
        Token::Space,
        Token::Int32(2),
        Token::End
    ]);
}

#[test]
fn whitespace_test() {
    assert_eq!(tokenize(String::from("\r\n  12/ 34\n")).unwrap(), vec![
        Token::Newline,
        Token::Int32(12),
        Token::Operator(Symbol::Slash),
        Token::Space,
        Token::Int32(34),
        Token::Newline,
        Token::End
    ]);
}
