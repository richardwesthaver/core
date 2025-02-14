//! ast.rs --- SXP internal representation
use crate::tok::Num;
// use crate::{Serializer, Result, Error};
// use serde::ser::Serialize;
// use serde::de::DeserializeOwned;

//  TODO 2023-07-09: implement serde.. this is a separate interface
//  which is to be used with macros.
#[derive(Clone, Eq, PartialEq)]
pub enum Form {
  Nil,
  Number(Num),
  String(String),
  Symbol(String),
  List(Vec<Form>),
}
