//! Handles building the AST from the tokens in the lexer.
//!
//! The parser here uses the Pratt parser, which is an extension of a
//! traditional recursive descent parser, except that it keeps track
//! of operator precedence to parse more complicated and nested
//! expressions. For boostrapping, this is a simple type of parser to
//! create.
//!
//! Since everything in Aero is an expression, the final result from
//! the main parse function is also an expression.

use std::iter::Peekable;

use ast::BinaryOp;
use ast::Expr;
use lexer::Symbol;
use lexer::Token;

type ExprResult = Result<Expr, String>;

/// Parse a vector of Tokens into an expression.
pub fn parse(tokens: Vec<Token>) -> ExprResult {
  let mut iter = tokens.iter().peekable();
  let mut expr_vec = vec![];

  // Keep parsing while there are still tokens.
  while let Some(&token) = iter.peek() {
    // Just pop off the token if it's whitespace.
    if token == &Token::Newline || token == &Token::Space {
      iter.next().unwrap();
      continue;
    }

    let result = parse_expr(&mut iter, 0);

    // On success, add the expression to the vector, otherwise return
    // the error.
    if let Ok(expr) = result {
      expr_vec.push(expr);
    } else {
      return result;
    }
  }

  // The top level expression is the special file expression.
  Ok(Expr::FileExpr(expr_vec))
}

// Get how much binding power the symbol has.
fn get_precedence(symbol: &Symbol) -> u8 {
  match symbol {
    &Symbol::Star | &Symbol::Slash => 110,
    &Symbol::Plus | &Symbol::Hyph => 100
  }
}

// Parse a single expression (up to a newline) with the Pratt parser.
fn parse_expr<'a, I>(iter: &mut Peekable<I>, precedence: u8) -> ExprResult
    where I: Iterator<Item=&'a Token> {
  // First, we'll parse the prefix operations, those which only need
  // to parse going forward and don't need the previous expression.
  let mut expr =
    match parse_prefix(iter) {
      Ok(prefix_expr) => prefix_expr,
      Err(message) => return Err(message)
    };

  // Keep parsing while there are still tokens.
  while let Some(&next_token) = iter.peek() {
    // Getting the preceedence of the next operator.
    let next_precedence =
      match next_token {
        &Token::Op(ref symbol) => get_precedence(symbol),
        // If this is a space we just drop it and continue the loop.
        &Token::Space => {
          iter.next().unwrap();
          continue;
        },
        // On a newline, the expression is over and we'll let the
        // function return the current expression.
        &Token::Newline => break,
        _ => return Err(format!("unexpected token \"{:?}\"", next_token))
      };

    // If our current precedence is higher than the next operator's
    // precedence, we start parsing the expression. Otherwise, we'll
    // go forward and parse the infix or postfix expressions from the
    // operator and update the current expression.
    if precedence >= next_precedence {
      break;
    } else {
      match parse_infix_postfix(iter, next_precedence, expr) {
        Ok(infix_expr) => expr = infix_expr,
        Err(message) => return Err(message)
      }
    }
  }

  Ok(expr)
}

// Parsing expressions which don't require the previous expression.
fn parse_prefix<'a, I>(iter: &mut Peekable<I>) -> ExprResult
    where I: Iterator<Item=&'a Token> {
  match iter.next() {
    Some(next_token) => match next_token {
      &Token::Int32Lit(num) => Ok(Expr::Int32LitExpr(num)),
      // We just throw out preceding spaces by calling this function
      // again.
      &Token::Space => parse_prefix(iter),
      _ => Err(format!("unexpected token \"{:?}\"", next_token))
    },
    None => Err(String::from("unexpected end of tokens"))
  }
}

// Parsing expressions which do require the previous expression.
fn parse_infix_postfix<'a, I>(iter: &mut Peekable<I>, precedence: u8,
    previous: Expr) -> ExprResult where I: Iterator<Item=&'a Token> {
  match iter.next().unwrap() {
    &Token::Op(ref symbol) => {
      let op =
        match symbol {
          &Symbol::Plus => BinaryOp::Add,
          &Symbol::Hyph => BinaryOp::Sub,
          &Symbol::Star => BinaryOp::Mul,
          &Symbol::Slash => BinaryOp::Div
        };

      let next =
        match parse_expr(iter, precedence) {
          Ok(expr) => expr,
          Err(message) => return Err(message)
        };

      Ok(Expr::BinaryExpr(op, Box::new(previous), Box::new(next)))
    },
    token => Err(format!("unexpected token \"{:?}\"", token))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use lexer::tokenize;

  #[test]
  fn test_empty() {
    let tokens = tokenize(String::from("")).unwrap();

    let ast = Expr::FileExpr(
      vec![]
    );

    assert_eq!(parse(tokens).unwrap(), ast);
  }

  #[test]
  fn test_basic_addition() {
    let tokens = tokenize(String::from("32 + 64")).unwrap();

    let ast = Expr::FileExpr(
      vec![
        Expr::BinaryExpr(
          BinaryOp::Add,
          Box::new(Expr::Int32LitExpr(32)),
          Box::new(Expr::Int32LitExpr(64))
        )
      ]
    );

    assert_eq!(parse(tokens).unwrap(), ast);
  }

  #[test]
  fn test_multiline() {
    let tokens = tokenize(String::from("1+ 2\n3 ")).unwrap();

    let ast = Expr::FileExpr(
      vec![
        Expr::BinaryExpr(
          BinaryOp::Add,
          Box::new(Expr::Int32LitExpr(1)),
          Box::new(Expr::Int32LitExpr(2))
        ),
        Expr::Int32LitExpr(3)
      ]
    );

    assert_eq!(parse(tokens).unwrap(), ast);
  }
}
