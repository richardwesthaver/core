use std::str::FromStr;
use sxp::Token;
#[test]
fn list_start_token() {
  assert_eq!(Token::from_str("(").unwrap(), Token::ListStart)
}
#[test]
fn list_end_token() {
  assert_eq!(Token::from_str(")").unwrap(), Token::ListEnd)
}
#[test]
fn sym_token() {
  assert_eq!(
    Token::from_str("foobar").unwrap(),
    Token::Sym("foobar".to_owned())
  )
}
#[test]
fn str_token() {
  assert_eq!(
    Token::from_str("\"foobar\"").unwrap(),
    Token::Str("foobar".to_owned())
  )
}
#[test]
fn num_token() {
  assert_eq!(
    Token::from_str("123").unwrap(),
    Token::Num("123".to_owned())
  )
}

//  TODO 2023-07-09: tests for floats,potnum,etc
