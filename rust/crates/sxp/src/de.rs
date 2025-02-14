//! de.rs --- SXP deserializer
use crate::read::{self, Fused, Reference};
use crate::{
  err::ErrorCode,
  fmt::{DefaultFormatter, ReadFormatter},
  Error, Result,
};
use alloc::string::String;
use alloc::vec::Vec;
use core::iter::FusedIterator;
use core::marker::PhantomData;
use serde::de::{self, Deserialize, Expected, Unexpected};
use serde::forward_to_deserialize_any;

pub use crate::read::{Read, SliceRead, StrRead};

#[cfg(feature = "std")]
pub use crate::read::IoRead;

pub struct Deserializer<R,F> {
  read: R,
  scratch: Vec<u8>,
  formatter: F,
  depth: u8,
  unbound: bool,
}

impl<'de, R: Read<'de>, F: ReadFormatter<'de>> Deserializer<R, F> {
  pub fn new(read: R, formatter: F) -> Self {
    Deserializer {
      read,
      scratch: Vec::new(),
      formatter,
      depth: 128,
      unbound: true,
    }
  }
}

#[cfg(feature = "std")]
impl<'de, R: Read<'de> + std::io::Read>
  Deserializer<read::IoRead<R>, DefaultFormatter>
{
  /// Creates a SXP deserializer from an `io::Read`.
  ///
  /// Reader-based deserializers do not support deserializing borrowed types
  /// like `&str`, since the `std::io::Read` trait has no non-copying methods
  /// -- everything it does involves copying bytes out of the data source.
  pub fn from_reader(reader: R) -> Self {
    let f = DefaultFormatter;
    Deserializer::new(read::IoRead::new(reader), f)
  }
}

impl<'a> Deserializer<read::SliceRead<'a>, DefaultFormatter> {
  /// Creates a SXP deserializer from a `&[u8]`.
  pub fn from_slice(bytes: &'a [u8]) -> Self {
    Deserializer::new(read::SliceRead::new(bytes), DefaultFormatter)
  }
}

impl<'a> Deserializer<read::StrRead<'a>, DefaultFormatter> {
  /// Creates a SXP deserializer from a `&str`.
  pub fn from_str(s: &'a str) -> Self {
    Deserializer::new(read::StrRead::new(s), DefaultFormatter)
  }
}

macro_rules! overflow {
  ($a:ident * 10 + $b:ident, $c:expr) => {
    match $c {
      c => $a >= c / 10 && ($a > c / 10 || $b > c % 10),
    }
  };
}

pub(crate) enum ParserNumber {
  F64(f64),
  U64(u64),
  I64(i64),
}

impl ParserNumber {
  fn visit<'de, V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    match self {
      ParserNumber::F64(x) => visitor.visit_f64(x),
      ParserNumber::U64(x) => visitor.visit_u64(x),
      ParserNumber::I64(x) => visitor.visit_i64(x),
    }
  }

  fn invalid_type(self, exp: &dyn Expected) -> Error {
    match self {
      ParserNumber::F64(x) => {
        de::Error::invalid_type(Unexpected::Float(x), exp)
      }
      ParserNumber::U64(x) => {
        de::Error::invalid_type(Unexpected::Unsigned(x), exp)
      }
      ParserNumber::I64(x) => {
        de::Error::invalid_type(Unexpected::Signed(x), exp)
      }
    }
  }
}

impl<'de, R: Read<'de>, F: ReadFormatter<'de>> Deserializer<R, F> {
  /// The `Deserializer::end` method should be called after a value has been
  /// fully deserialized. This allows the `Deserializer` to validate that the
  /// input stream is at the end or that it only has trailing whitespace.
  pub fn end(&mut self) -> Result<()> {
    match e!(self.parse_whitespace()) {
      Some(_) => Err(self.peek_error(ErrorCode::TrailingCharacters)),
      None => Ok(()),
    }
  }

  /// Turn a SXP deserializer into an iterator over values of type T.
  pub fn into_iter<T>(self) -> StreamDeserializer<'de, R, F, T>
  where
    T: de::Deserialize<'de>,
  {
    // This cannot be an implementation of std::iter::IntoIterator because
    // we need the caller to choose what T is.
    let offset = self.read.byte_offset();
    StreamDeserializer {
      de: self,
      offset,
      failed: false,
      output: PhantomData,
      lifetime: PhantomData,
    }
  }

  #[cfg(feature = "unbound")]
  /// Parse arbitrarily deep SXP structures without any consideration for
  /// overflowing the stack.
  ///
  /// You will want to provide some other way to protect against stack
  /// overflows, such as by wrapping your Deserializer in the dynamically
  /// growing stack adapter provided by the serde_stacker crate. Additionally
  /// you will need to be careful around other recursive operations on the
  /// parsed result which may overflow the stack after deserialization has
  /// completed, including, but not limited to, Display and Debug and Drop
  /// impls.
  ///
  /// *This method is only available if sxp is built with the
  /// `"unbound"` feature.*
  pub fn disable_recursion_limit(&mut self) {
    self.unbound = true;
  }

  pub(crate) fn peek(&mut self) -> Result<Option<u8>> {
    self.read.peek()
  }

  fn peek_or_null(&mut self) -> Result<u8> {
    Ok(e!(self.peek()).unwrap_or(b'\x00'))
  }

  fn eat_char(&mut self) {
    self.read.discard();
  }

  fn next_char(&mut self) -> Result<Option<u8>> {
    self.read.next()
  }

  fn next_char_or_null(&mut self) -> Result<u8> {
    Ok(e!(self.next_char()).unwrap_or(b'\x00'))
  }

  /// Error caused by a byte from next_char().
  #[cold]
  fn error(&self, reason: ErrorCode) -> Error {
    let position = self.read.position();
    Error::syntax(reason, position.line, position.column)
  }

  /// Error caused by a byte from peek().
  #[cold]
  fn peek_error(&self, reason: ErrorCode) -> Error {
    let position = self.read.peek_position();
    Error::syntax(reason, position.line, position.column)
  }

  /// Returns the first non-whitespace byte without consuming it, or `None` if
  /// EOF is encountered.
  fn parse_whitespace(&mut self) -> Result<Option<u8>> {
    loop {
      match e!(self.peek()) {
        Some(b' ' | b'\n' | b'\t' | b'\r') => {
          self.eat_char();
        }
        other => {
          return Ok(other);
        }
      }
    }
  }

  #[cold]
  fn peek_invalid_type(&mut self, exp: &dyn Expected) -> Error {
    let err = match self.peek_or_null().unwrap_or(b'\x00') {
      b'n' => {
        self.eat_char();
        if let Err(err) = self.parse_ident(b"il") {
          return err;
        }
        de::Error::invalid_type(Unexpected::Unit, exp)
      }
      //  TODO 2023-07-14 : use read-symbol
      b't' => {
        self.eat_char();
        if let Err(err) = self.parse_ident(b"rue") {
          return err;
        }
        de::Error::invalid_type(Unexpected::Bool(true), exp)
      }
      b'f' => {
        self.eat_char();
        if let Err(err) = self.parse_ident(b"") {
          return err;
        }
        de::Error::invalid_type(Unexpected::Bool(false), exp)
      }
      b'-' => {
        self.eat_char();
        match self.parse_any_number(false) {
          Ok(n) => n.invalid_type(exp),
          Err(err) => return err,
        }
      }
      b'0'..=b'9' => match self.parse_any_number(true) {
        Ok(n) => n.invalid_type(exp),
        Err(err) => return err,
      },
      b'"' => {
        self.eat_char();
        self.scratch.clear();
        match self.read.parse_str(&mut self.scratch) {
          Ok(s) => de::Error::invalid_type(Unexpected::Str(&s), exp),
          Err(err) => return err,
        }
      }
      b'(' => de::Error::invalid_type(Unexpected::Seq, exp),
      _ => self.peek_error(ErrorCode::ExpectedSomeValue),
    };

    self.fix_position(err)
  }

  pub(crate) fn deserialize_number<'any, V>(
    &mut self,
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'any>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'-' => {
        self.eat_char();
        e!(self.parse_integer(false)).visit(visitor)
      }
      b'0'..=b'9' => e!(self.parse_integer(true)).visit(visitor),
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  pub(crate) fn do_deserialize_i128<'any, V>(
    &mut self,
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'any>,
  {
    let mut buf = String::new();

    match e!(self.parse_whitespace()) {
      Some(b'-') => {
        self.eat_char();
        buf.push('-');
      }
      Some(_) => {}
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    e!(self.scan_integer128(&mut buf));

    let value = match buf.parse() {
      Ok(int) => visitor.visit_i128(int),
      Err(_) => {
        return Err(self.error(ErrorCode::NumberOutOfRange));
      }
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  pub(crate) fn do_deserialize_u128<'any, V>(
    &mut self,
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'any>,
  {
    match e!(self.parse_whitespace()) {
      Some(b'-') => {
        return Err(self.peek_error(ErrorCode::NumberOutOfRange));
      }
      Some(_) => {}
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    }

    let mut buf = String::new();
    e!(self.scan_integer128(&mut buf));

    let value = match buf.parse() {
      Ok(int) => visitor.visit_u128(int),
      Err(_) => {
        return Err(self.error(ErrorCode::NumberOutOfRange));
      }
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  fn scan_integer128(&mut self, buf: &mut String) -> Result<()> {
    match e!(self.next_char_or_null()) {
      b'0' => {
        buf.push('0');
        // There can be only one leading '0'.
        match e!(self.peek_or_null()) {
          b'0'..=b'9' => Err(self.peek_error(ErrorCode::InvalidNumber)),
          _ => Ok(()),
        }
      }
      c @ b'1'..=b'9' => {
        buf.push(c as char);
        while let c @ b'0'..=b'9' = e!(self.peek_or_null()) {
          self.eat_char();
          buf.push(c as char);
        }
        Ok(())
      }
      _ => Err(self.error(ErrorCode::InvalidNumber)),
    }
  }

  #[cold]
  fn fix_position(&self, err: Error) -> Error {
    err.fix_position(move |code| self.error(code))
  }

  fn parse_ident(&mut self, ident: &[u8]) -> Result<()> {
    for expected in ident {
      match e!(self.next_char()) {
        None => {
          return Err(self.error(ErrorCode::EofWhileParsingValue));
        }
        Some(next) => {
          if next != *expected {
            return Err(self.error(ErrorCode::ExpectedSomeSymbol));
          }
        }
      }
    }

    Ok(())
  }

  fn parse_integer(&mut self, positive: bool) -> Result<ParserNumber> {
    let next = match e!(self.next_char()) {
      Some(b) => b,
      None => {
        return Err(self.error(ErrorCode::EofWhileParsingValue));
      }
    };

    match next {
      b'0' => {
        // There can be only one leading '0'.
        match e!(self.peek_or_null()) {
          b'0'..=b'9' => Err(self.peek_error(ErrorCode::InvalidNumber)),
          _ => self.parse_number(positive, 0),
        }
      }
      c @ b'1'..=b'9' => {
        let mut significand = (c - b'0') as u64;

        loop {
          match e!(self.peek_or_null()) {
            c @ b'0'..=b'9' => {
              let digit = (c - b'0') as u64;

              // We need to be careful with overflow. If we can,
              // try to keep the number as a `u64` until we grow
              // too large. At that point, switch to parsing the
              // value as a `f64`.
              if overflow!(significand * 10 + digit, u64::max_value()) {
                return Ok(ParserNumber::F64(e!(
                  self.parse_long_integer(positive, significand),
                )));
              }

              self.eat_char();
              significand = significand * 10 + digit;
            }
            _ => {
              return self.parse_number(positive, significand);
            }
          }
        }
      }
      _ => Err(self.error(ErrorCode::InvalidNumber)),
    }
  }

  fn parse_number(
    &mut self,
    positive: bool,
    significand: u64,
  ) -> Result<ParserNumber> {
    Ok(match e!(self.peek_or_null()) {
      b'.' => {
        ParserNumber::F64(e!(self.parse_decimal(positive, significand, 0)))
      }
      b'e' | b'E' => {
        ParserNumber::F64(e!(self.parse_exponent(positive, significand, 0)))
      }
      _ => {
        if positive {
          ParserNumber::U64(significand)
        } else {
          let neg = (significand as i64).wrapping_neg();

          // Convert into a float if we underflow, or on `-0`.
          if neg >= 0 {
            ParserNumber::F64(-(significand as f64))
          } else {
            ParserNumber::I64(neg)
          }
        }
      }
    })
  }

  fn parse_decimal(
    &mut self,
    positive: bool,
    mut significand: u64,
    exponent_before_decimal_point: i32,
  ) -> Result<f64> {
    self.eat_char();

    let mut exponent_after_decimal_point = 0;
    while let c @ b'0'..=b'9' = e!(self.peek_or_null()) {
      let digit = (c - b'0') as u64;

      if overflow!(significand * 10 + digit, u64::max_value()) {
        let exponent =
          exponent_before_decimal_point + exponent_after_decimal_point;
        return self.parse_decimal_overflow(positive, significand, exponent);
      }

      self.eat_char();
      significand = significand * 10 + digit;
      exponent_after_decimal_point -= 1;
    }

    // Error if there is not at least one digit after the decimal point.
    if exponent_after_decimal_point == 0 {
      match e!(self.peek()) {
        Some(_) => return Err(self.peek_error(ErrorCode::InvalidNumber)),
        None => return Err(self.peek_error(ErrorCode::EofWhileParsingValue)),
      }
    }

    let exponent = exponent_before_decimal_point + exponent_after_decimal_point;
    match e!(self.peek_or_null()) {
      b'e' | b'E' => self.parse_exponent(positive, significand, exponent),
      _ => self.f64_from_parts(positive, significand, exponent),
    }
  }

  fn parse_exponent(
    &mut self,
    positive: bool,
    significand: u64,
    starting_exp: i32,
  ) -> Result<f64> {
    self.eat_char();

    let positive_exp = match e!(self.peek_or_null()) {
      b'+' => {
        self.eat_char();
        true
      }
      b'-' => {
        self.eat_char();
        false
      }
      _ => true,
    };

    let next = match e!(self.next_char()) {
      Some(b) => b,
      None => {
        return Err(self.error(ErrorCode::EofWhileParsingValue));
      }
    };

    // Make sure a digit follows the exponent place.
    let mut exp = match next {
      c @ b'0'..=b'9' => (c - b'0') as i32,
      _ => {
        return Err(self.error(ErrorCode::InvalidNumber));
      }
    };

    while let c @ b'0'..=b'9' = e!(self.peek_or_null()) {
      self.eat_char();
      let digit = (c - b'0') as i32;

      if overflow!(exp * 10 + digit, i32::max_value()) {
        let zero_significand = significand == 0;
        return self.parse_exponent_overflow(
          positive,
          zero_significand,
          positive_exp,
        );
      }

      exp = exp * 10 + digit;
    }

    let final_exp = if positive_exp {
      starting_exp.saturating_add(exp)
    } else {
      starting_exp.saturating_sub(exp)
    };

    self.f64_from_parts(positive, significand, final_exp)
  }

  fn f64_from_parts(
    &mut self,
    positive: bool,
    significand: u64,
    mut exponent: i32,
  ) -> Result<f64> {
    let mut f = significand as f64;
    loop {
      match POW10.get(exponent.wrapping_abs() as usize) {
        Some(&pow) => {
          if exponent >= 0 {
            f *= pow;
            if f.is_infinite() {
              return Err(self.error(ErrorCode::NumberOutOfRange));
            }
          } else {
            f /= pow;
          }
          break;
        }
        None => {
          if f == 0.0 {
            break;
          }
          if exponent >= 0 {
            return Err(self.error(ErrorCode::NumberOutOfRange));
          }
          f /= 1e308;
          exponent += 308;
        }
      }
    }
    Ok(if positive { f } else { -f })
  }

  #[cold]
  #[inline(never)]
  fn parse_long_integer(
    &mut self,
    positive: bool,
    significand: u64,
  ) -> Result<f64> {
    let mut exponent = 0;
    loop {
      match e!(self.peek_or_null()) {
        b'0'..=b'9' => {
          self.eat_char();
          // This could overflow... if your integer is gigabytes long.
          // Ignore that possibility.
          exponent += 1;
        }
        b'.' => {
          return self.parse_decimal(positive, significand, exponent);
        }
        b'e' | b'E' => {
          return self.parse_exponent(positive, significand, exponent);
        }
        _ => {
          return self.f64_from_parts(positive, significand, exponent);
        }
      }
    }
  }

  #[cold]
  #[inline(never)]
  fn parse_decimal_overflow(
    &mut self,
    positive: bool,
    significand: u64,
    exponent: i32,
  ) -> Result<f64> {
    // The next multiply/add would overflow, so just ignore all further
    // digits.
    while let b'0'..=b'9' = e!(self.peek_or_null()) {
      self.eat_char();
    }

    match e!(self.peek_or_null()) {
      b'e' | b'E' => self.parse_exponent(positive, significand, exponent),
      _ => self.f64_from_parts(positive, significand, exponent),
    }
  }

  // This cold code should not be inlined into the middle of the hot
  // exponent-parsing loop above.
  #[cold]
  #[inline(never)]
  fn parse_exponent_overflow(
    &mut self,
    positive: bool,
    zero_significand: bool,
    positive_exp: bool,
  ) -> Result<f64> {
    // Error instead of +/- infinity.
    if !zero_significand && positive_exp {
      return Err(self.error(ErrorCode::NumberOutOfRange));
    }

    while let b'0'..=b'9' = e!(self.peek_or_null()) {
      self.eat_char();
    }
    Ok(if positive { 0.0 } else { -0.0 })
  }

  fn parse_any_signed_number(&mut self) -> Result<ParserNumber> {
    let peek = match e!(self.peek()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'-' => {
        self.eat_char();
        self.parse_any_number(false)
      }
      b'0'..=b'9' => self.parse_any_number(true),
      _ => Err(self.peek_error(ErrorCode::InvalidNumber)),
    };

    let value = match e!(self.peek()) {
      Some(_) => Err(self.peek_error(ErrorCode::InvalidNumber)),
      None => value,
    };

    match value {
      Ok(value) => Ok(value),
      // The de::Error impl creates errors with unknown line and column.
      // Fill in the position here by looking at the current index in the
      // input. There is no way to tell whether this should call `error`
      // or `peek_error` so pick the one that seems correct more often.
      // Worst case, the position is off by one character.
      Err(err) => Err(self.fix_position(err)),
    }
  }

  fn parse_any_number(&mut self, positive: bool) -> Result<ParserNumber> {
    self.parse_integer(positive)
  }

  fn end_seq(&mut self) -> Result<()> {
    match e!(self.parse_whitespace()) {
      Some(b')') => {
        self.eat_char();
        Ok(())
      }
      Some(_) => {
        self.eat_char();
        match self.parse_whitespace() {
          _ => Err(self.peek_error(ErrorCode::TrailingCharacters)),
        }
      }
      None => Err(self.peek_error(ErrorCode::EofWhileParsingList)),
    }
  }

  fn end_map(&mut self) -> Result<()> {
    match e!(self.parse_whitespace()) {
      Some(b')') => {
        self.eat_char();
        Ok(())
      }
      Some(_) => Err(self.peek_error(ErrorCode::TrailingCharacters)),
      None => Err(self.peek_error(ErrorCode::EofWhileParsingObject)),
    }
  }

  fn ignore_value(&mut self) -> Result<()> {
    self.scratch.clear();
    let mut enclosing = None;

    loop {
      let peek = match e!(self.parse_whitespace()) {
        Some(b) => b,
        None => {
          return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
        }
      };

      let frame = match peek {
        b'n' => {
          self.eat_char();
          e!(self.parse_ident(b"il"));
          None
        }
        b't' => {
          // TODO
          self.eat_char();
          e!(self.parse_ident(b"rue"));
          None
        }
        b'f' => {
          self.eat_char();
          e!(self.parse_ident(b"alse"));
          None
        }
        b'-' => {
          self.eat_char();
          e!(self.ignore_integer());
          None
        }
        b'0'..=b'9' => {
          e!(self.ignore_integer());
          None
        }
        b'"' => {
          self.eat_char();
          e!(self.read.ignore_str());
          None
        }
        frame @ b'(' => {
          self.scratch.extend(enclosing.take());
          self.eat_char();
          Some(frame)
        }
        _ => return Err(self.peek_error(ErrorCode::ExpectedSomeValue)),
      };

      let mut frame = match frame {
        Some(frame) => frame,
        None => match enclosing.take() {
          Some(frame) => frame,
          None => match self.scratch.pop() {
            Some(frame) => frame,
            None => return Ok(()),
          },
        },
      };

      loop {
        match e!(self.parse_whitespace()) {
          Some(b')') if frame == b'(' => {}
          Some(_) => {
            break;
          }
          None => {
            return Err(self.peek_error(match frame {
              b'(' => ErrorCode::EofWhileParsingList,
              _ => unreachable!(),
            }));
          }
        }

        self.eat_char();
        frame = match self.scratch.pop() {
          Some(frame) => frame,
          None => return Ok(()),
        };
      }

      if frame == b'(' {
        match e!(self.parse_whitespace()) {
          Some(b'"') => self.eat_char(),
          Some(_) => return Err(self.peek_error(ErrorCode::KeyMustBeASymbol)),
          None => {
            return Err(self.peek_error(ErrorCode::EofWhileParsingObject))
          }
        }
        e!(self.read.ignore_str());
        match e!(self.parse_whitespace()) {
          Some(b':') => self.eat_char(),
          Some(_) => {
            return Err(self.peek_error(ErrorCode::EofWhileParsingObject))
          }
          None => {
            return Err(self.peek_error(ErrorCode::EofWhileParsingObject))
          }
        }
      }

      enclosing = Some(frame);
    }
  }

  fn ignore_integer(&mut self) -> Result<()> {
    match e!(self.next_char_or_null()) {
      b'0' => {
        // There can be only one leading '0'.
        if let b'0'..=b'9' = e!(self.peek_or_null()) {
          return Err(self.peek_error(ErrorCode::InvalidNumber));
        }
      }
      b'1'..=b'9' => {
        while let b'0'..=b'9' = e!(self.peek_or_null()) {
          self.eat_char();
        }
      }
      _ => {
        return Err(self.error(ErrorCode::InvalidNumber));
      }
    }

    match e!(self.peek_or_null()) {
      b'.' => self.ignore_decimal(),
      b'e' | b'E' => self.ignore_exponent(),
      _ => Ok(()),
    }
  }

  fn ignore_decimal(&mut self) -> Result<()> {
    self.eat_char();

    let mut at_least_one_digit = false;
    while let b'0'..=b'9' = e!(self.peek_or_null()) {
      self.eat_char();
      at_least_one_digit = true;
    }

    if !at_least_one_digit {
      return Err(self.peek_error(ErrorCode::InvalidNumber));
    }

    match e!(self.peek_or_null()) {
      b'e' | b'E' => self.ignore_exponent(),
      _ => Ok(()),
    }
  }

  fn ignore_exponent(&mut self) -> Result<()> {
    self.eat_char();

    match e!(self.peek_or_null()) {
      b'+' | b'-' => self.eat_char(),
      _ => {}
    }

    // Make sure a digit follows the exponent place.
    match e!(self.next_char_or_null()) {
      b'0'..=b'9' => {}
      _ => {
        return Err(self.error(ErrorCode::InvalidNumber));
      }
    }

    while let b'0'..=b'9' = e!(self.peek_or_null()) {
      self.eat_char();
    }

    Ok(())
  }
}

#[cfg(not(feature = "float_roundtrip"))]
static POW10: [f64; 309] = [
  1e000, 1e001, 1e002, 1e003, 1e004, 1e005, 1e006, 1e007, 1e008, 1e009, //
  1e010, 1e011, 1e012, 1e013, 1e014, 1e015, 1e016, 1e017, 1e018, 1e019, //
  1e020, 1e021, 1e022, 1e023, 1e024, 1e025, 1e026, 1e027, 1e028, 1e029, //
  1e030, 1e031, 1e032, 1e033, 1e034, 1e035, 1e036, 1e037, 1e038, 1e039, //
  1e040, 1e041, 1e042, 1e043, 1e044, 1e045, 1e046, 1e047, 1e048, 1e049, //
  1e050, 1e051, 1e052, 1e053, 1e054, 1e055, 1e056, 1e057, 1e058, 1e059, //
  1e060, 1e061, 1e062, 1e063, 1e064, 1e065, 1e066, 1e067, 1e068, 1e069, //
  1e070, 1e071, 1e072, 1e073, 1e074, 1e075, 1e076, 1e077, 1e078, 1e079, //
  1e080, 1e081, 1e082, 1e083, 1e084, 1e085, 1e086, 1e087, 1e088, 1e089, //
  1e090, 1e091, 1e092, 1e093, 1e094, 1e095, 1e096, 1e097, 1e098, 1e099, //
  1e100, 1e101, 1e102, 1e103, 1e104, 1e105, 1e106, 1e107, 1e108, 1e109, //
  1e110, 1e111, 1e112, 1e113, 1e114, 1e115, 1e116, 1e117, 1e118, 1e119, //
  1e120, 1e121, 1e122, 1e123, 1e124, 1e125, 1e126, 1e127, 1e128, 1e129, //
  1e130, 1e131, 1e132, 1e133, 1e134, 1e135, 1e136, 1e137, 1e138, 1e139, //
  1e140, 1e141, 1e142, 1e143, 1e144, 1e145, 1e146, 1e147, 1e148, 1e149, //
  1e150, 1e151, 1e152, 1e153, 1e154, 1e155, 1e156, 1e157, 1e158, 1e159, //
  1e160, 1e161, 1e162, 1e163, 1e164, 1e165, 1e166, 1e167, 1e168, 1e169, //
  1e170, 1e171, 1e172, 1e173, 1e174, 1e175, 1e176, 1e177, 1e178, 1e179, //
  1e180, 1e181, 1e182, 1e183, 1e184, 1e185, 1e186, 1e187, 1e188, 1e189, //
  1e190, 1e191, 1e192, 1e193, 1e194, 1e195, 1e196, 1e197, 1e198, 1e199, //
  1e200, 1e201, 1e202, 1e203, 1e204, 1e205, 1e206, 1e207, 1e208, 1e209, //
  1e210, 1e211, 1e212, 1e213, 1e214, 1e215, 1e216, 1e217, 1e218, 1e219, //
  1e220, 1e221, 1e222, 1e223, 1e224, 1e225, 1e226, 1e227, 1e228, 1e229, //
  1e230, 1e231, 1e232, 1e233, 1e234, 1e235, 1e236, 1e237, 1e238, 1e239, //
  1e240, 1e241, 1e242, 1e243, 1e244, 1e245, 1e246, 1e247, 1e248, 1e249, //
  1e250, 1e251, 1e252, 1e253, 1e254, 1e255, 1e256, 1e257, 1e258, 1e259, //
  1e260, 1e261, 1e262, 1e263, 1e264, 1e265, 1e266, 1e267, 1e268, 1e269, //
  1e270, 1e271, 1e272, 1e273, 1e274, 1e275, 1e276, 1e277, 1e278, 1e279, //
  1e280, 1e281, 1e282, 1e283, 1e284, 1e285, 1e286, 1e287, 1e288, 1e289, //
  1e290, 1e291, 1e292, 1e293, 1e294, 1e295, 1e296, 1e297, 1e298, 1e299, //
  1e300, 1e301, 1e302, 1e303, 1e304, 1e305, 1e306, 1e307, 1e308,
];

macro_rules! deserialize_number {
  ($method:ident) => {
    deserialize_number!($method, deserialize_number);
  };

  ($method:ident, $using:ident) => {
    fn $method<V>(self, visitor: V) -> Result<V::Value>
    where
      V: de::Visitor<'de>,
    {
      self.$using(visitor)
    }
  };
}

macro_rules! if_checking_recursion_limit {
    ($($body:tt)*) => {
        $($body)*
    };
}

macro_rules! check_recursion {
    ($this:ident $($body:tt)*) => {
        if_checking_recursion_limit! {
          $this.depth -= 1;
            if $this.depth == 0 {
                return Err($this.peek_error(ErrorCode::RecursionLimitExceeded));
            }
        }

        $this $($body)*

        if_checking_recursion_limit! {
            $this.depth += 1;
        }
    };
}

impl<'de, 'a, R: Read<'de>, F: ReadFormatter<'de>> de::Deserializer<'de>
  for &'a mut Deserializer<R, F>
{
  type Error = Error;

  #[inline]
  fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'n' => {
        self.eat_char();
        e!(self.parse_ident(b"ull"));
        visitor.visit_unit()
      }
      b't' => {
        self.eat_char();
        e!(self.parse_ident(b"rue"));
        visitor.visit_bool(true)
      }
      b'f' => {
        self.eat_char();
        e!(self.parse_ident(b"alse"));
        visitor.visit_bool(false)
      }
      b'-' => {
        self.eat_char();
        e!(self.parse_any_number(false)).visit(visitor)
      }
      b'0'..=b'9' => e!(self.parse_any_number(true)).visit(visitor),
      b'"' => {
        self.eat_char();
        self.scratch.clear();
        match e!(self.read.parse_str(&mut self.scratch)) {
          Reference::Borrowed(s) => visitor.visit_borrowed_str(s),
          Reference::Copied(s) => visitor.visit_str(s),
        }
      }
      b'(' => {
        check_recursion! {
            self.eat_char();
            let ret = visitor.visit_seq(SeqAccess::new(self));
        }

        match (ret, self.end_seq()) {
          (Ok(ret), Ok(())) => Ok(ret),
          (Err(err), _) | (_, Err(err)) => Err(err),
        }
      }
      _ => Err(self.peek_error(ErrorCode::ExpectedSomeValue)),
    };

    match value {
      Ok(value) => Ok(value),
      // The de::Error impl creates errors with unknown line and column.
      // Fill in the position here by looking at the current index in the
      // input. There is no way to tell whether this should call `error`
      // or `peek_error` so pick the one that seems correct more often.
      // Worst case, the position is off by one character.
      Err(err) => Err(self.fix_position(err)),
    }
  }

  fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b't' => {
        self.eat_char();
        e!(self.parse_ident(b"rue"));
        visitor.visit_bool(true)
      }
      b'f' => {
        self.eat_char();
        e!(self.parse_ident(b"alse"));
        visitor.visit_bool(false)
      }
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  deserialize_number!(deserialize_i8);
  deserialize_number!(deserialize_i16);
  deserialize_number!(deserialize_i32);
  deserialize_number!(deserialize_i64);
  deserialize_number!(deserialize_u8);
  deserialize_number!(deserialize_u16);
  deserialize_number!(deserialize_u32);
  deserialize_number!(deserialize_u64);
  deserialize_number!(deserialize_f32);
  deserialize_number!(deserialize_f64);

  deserialize_number!(deserialize_i128, do_deserialize_i128);
  deserialize_number!(deserialize_u128, do_deserialize_u128);

  fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.deserialize_str(visitor)
  }

  fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'"' => {
        self.eat_char();
        self.scratch.clear();
        match e!(self.read.parse_str(&mut self.scratch)) {
          Reference::Borrowed(s) => visitor.visit_borrowed_str(s),
          Reference::Copied(s) => visitor.visit_str(s),
        }
      }
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.deserialize_str(visitor)
  }

  /// Parses a SXP string as bytes. Note that this function does not check
  /// whether the bytes represent a valid UTF-8 string.
  ///
  /// The relevant part of the SXP specification is Section 8.2 of [RFC
  /// 7159]:
  ///
  /// > When all the strings represented in a SXP text are composed entirely
  /// > of Unicode characters (however escaped), then that SXP text is
  /// > interoperable in the sense that all software implementations that
  /// > parse it will agree on the contents of names and of string values in
  /// > objects and arrays.
  /// >
  /// > However, the ABNF in this specification allows member names and string
  /// > values to contain bit sequences that cannot encode Unicode characters;
  /// > for example, "\uDEAD" (a single unpaired UTF-16 surrogate). Instances
  /// > of this have been observed, for example, when a library truncates a
  /// > UTF-16 string without checking whether the truncation split a
  /// > surrogate pair.  The behavior of software that receives SXP texts
  /// > containing such values is unpredictable; for example, implementations
  /// > might return different values for the length of a string value or even
  /// > suffer fatal runtime exceptions.
  ///
  /// [RFC 7159]: https://tools.ietf.org/html/rfc7159
  ///
  /// The behavior of sxp is specified to fail on non-UTF-8 strings
  /// when deserializing into Rust UTF-8 string types such as String, and
  /// succeed with non-UTF-8 bytes when deserializing using this method.
  ///
  /// Escape sequences are processed as usual, and for `\uXXXX` escapes it is
  /// still checked if the hex number represents a valid Unicode code point.
  fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'"' => {
        self.eat_char();
        self.scratch.clear();
        match e!(self.read.parse_str_raw(&mut self.scratch)) {
          Reference::Borrowed(b) => visitor.visit_borrowed_bytes(b),
          Reference::Copied(b) => visitor.visit_bytes(b),
        }
      }
      b'(' => self.deserialize_seq(visitor),
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  #[inline]
  fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.deserialize_bytes(visitor)
  }

  /// Parses a `null` as a None, and any other values as a `Some(...)`.
  #[inline]
  fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    match e!(self.parse_whitespace()) {
      Some(b'n') => {
        self.eat_char();
        e!(self.parse_ident(b"ull"));
        visitor.visit_none()
      }
      _ => visitor.visit_some(self),
    }
  }

  fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'n' => {
        self.eat_char();
        e!(self.parse_ident(b"il"));
        visitor.visit_unit()
      }
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  fn deserialize_unit_struct<V>(
    self,
    _name: &'static str,
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.deserialize_unit(visitor)
  }

  /// Parses a newtype struct as the underlying value.
  #[inline]
  fn deserialize_newtype_struct<V>(
    self,
    name: &str,
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let _ = name;
    visitor.visit_newtype_struct(self)
  }

  fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'(' => {
        check_recursion! {
            self.eat_char();
            let ret = visitor.visit_seq(SeqAccess::new(self));
        }

        match (ret, self.end_seq()) {
          (Ok(ret), Ok(())) => Ok(ret),
          (Err(err), _) | (_, Err(err)) => Err(err),
        }
      }
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.deserialize_seq(visitor)
  }

  fn deserialize_tuple_struct<V>(
    self,
    _name: &'static str,
    _len: usize,
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.deserialize_seq(visitor)
  }

  fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'(' => {
        check_recursion! {
            self.eat_char();
            let ret = visitor.visit_map(MapAccess::new(self));
        }

        match (ret, self.end_map()) {
          (Ok(ret), Ok(())) => Ok(ret),
          (Err(err), _) | (_, Err(err)) => Err(err),
        }
      }
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  fn deserialize_struct<V>(
    self,
    _name: &'static str,
    _fields: &'static [&'static str],
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let peek = match e!(self.parse_whitespace()) {
      Some(b) => b,
      None => {
        return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
      }
    };

    let value = match peek {
      b'(' => {
        check_recursion! {
            self.eat_char();
            let ret = visitor.visit_seq(SeqAccess::new(self));
        }

        match (ret, self.end_seq()) {
          (Ok(ret), Ok(())) => Ok(ret),
          (Err(err), _) | (_, Err(err)) => Err(err),
        }
      }
      _ => Err(self.peek_invalid_type(&visitor)),
    };

    match value {
      Ok(value) => Ok(value),
      Err(err) => Err(self.fix_position(err)),
    }
  }

  /// Parses an enum as an object like `{"$KEY":$VALUE}`, where $VALUE is either
  /// a straight value, a `[..]`, or a `{..}`.
  #[inline]
  fn deserialize_enum<V>(
    self,
    _name: &str,
    _variants: &'static [&'static str],
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    match e!(self.parse_whitespace()) {
      Some(b'(') => {
        check_recursion! {
            self.eat_char();
            let value = e!(visitor.visit_enum(VariantAccess::new(self)));
        }

        match e!(self.parse_whitespace()) {
          Some(b')') => {
            self.eat_char();
            Ok(value)
          }
          Some(_) => Err(self.error(ErrorCode::ExpectedSomeValue)),
          None => Err(self.error(ErrorCode::EofWhileParsingObject)),
        }
      }
      Some(b'"') => visitor.visit_enum(UnitVariantAccess::new(self)),
      Some(_) => visitor.visit_enum(UnitVariantAccess::new(self)),
      None => Err(self.peek_error(ErrorCode::EofWhileParsingValue)),
    }
  }

  fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.deserialize_str(visitor)
  }

  fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    e!(self.ignore_value());
    visitor.visit_unit()
  }
}

struct SeqAccess<'a, R: 'a, F: 'a> {
  de: &'a mut Deserializer<R, F>,
  first: bool,
}

impl<'a, R: 'a, F: 'a> SeqAccess<'a, R, F> {
  fn new(de: &'a mut Deserializer<R, F>) -> Self {
    SeqAccess { de, first: true }
  }
}

impl<'de, 'a, R: Read<'de> + 'a, F: ReadFormatter<'de> + 'a> de::SeqAccess<'de>
  for SeqAccess<'a, R, F>
{
  type Error = Error;

  fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
  where
    T: de::DeserializeSeed<'de>,
  {
    let peek = match e!(self.de.parse_whitespace()) {
      Some(b')') => {
        return Ok(None);
      }
      Some(b) => {
        if self.first {
          self.first = false;
        }
        Some(b)
      }
      None => {
        return Err(self.de.peek_error(ErrorCode::EofWhileParsingList));
      }
    };

    match peek {
      Some(_) => Ok(Some(e!(seed.deserialize(&mut *self.de)))),
      None => Err(self.de.peek_error(ErrorCode::EofWhileParsingValue)),
    }
  }
}

struct MapAccess<'a, R: 'a, F> {
  de: &'a mut Deserializer<R, F>,
  first: bool,
}

impl<'a, R: 'a, F> MapAccess<'a, R, F> {
  fn new(de: &'a mut Deserializer<R, F>) -> Self {
    MapAccess { de, first: true }
  }
}

impl<'de, 'a, R: Read<'de> + 'a, F: for<'r> ReadFormatter<'r>> de::MapAccess<'de>
  for MapAccess<'a, R, F>
{
  type Error = Error;

  fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
  where
    K: de::DeserializeSeed<'de>,
  {
    let peek = match e!(self.de.parse_whitespace()) {
      Some(b')') => {
        return Ok(None);
      }
      Some(b) => {
        if self.first {
          self.first = false;
        }
        Some(b)
      }
      None => {
        return Err(self.de.peek_error(ErrorCode::EofWhileParsingObject));
      }
    };

    match peek {
      Some(b'"') => seed.deserialize(MapKey { de: &mut *self.de }).map(Some),
      Some(b')') => Err(self.de.peek_error(ErrorCode::TrailingCharacters)),
      Some(_) => Err(self.de.peek_error(ErrorCode::KeyMustBeASymbol)),
      None => Err(self.de.peek_error(ErrorCode::EofWhileParsingValue)),
    }
  }

  fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
  where
    V: de::DeserializeSeed<'de>,
  {
    e!(self.de.parse_whitespace());
    seed.deserialize(&mut *self.de)
  }
}

struct VariantAccess<'a, R: 'a, F: 'a> {
  de: &'a mut Deserializer<R, F>,
}

impl<'a, R: 'a, F: 'a> VariantAccess<'a, R, F> {
  fn new(de: &'a mut Deserializer<R, F>) -> Self {
    VariantAccess { de }
  }
}

impl<'de, 'a, R: Read<'de> + 'a, F: ReadFormatter<'de> + 'a> de::EnumAccess<'de>
  for VariantAccess<'a, R, F>
{
  type Error = Error;
  type Variant = Self;

  fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self)>
  where
    V: de::DeserializeSeed<'de>,
  {
    let val = e!(seed.deserialize(&mut *self.de));
    e!(self.de.parse_whitespace());
    Ok((val, self))
  }
}

impl<'de, 'a, R: Read<'de> + 'a, F: ReadFormatter<'de> + 'a> de::VariantAccess<'de>
  for VariantAccess<'a, R, F>
{
  type Error = Error;

  fn unit_variant(self) -> Result<()> {
    de::Deserialize::deserialize(self.de)
  }

  fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
  where
    T: de::DeserializeSeed<'de>,
  {
    seed.deserialize(self.de)
  }

  fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    de::Deserializer::deserialize_seq(self.de, visitor)
  }

  fn struct_variant<V>(
    self,
    fields: &'static [&'static str],
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    de::Deserializer::deserialize_struct(self.de, "", fields, visitor)
  }
}

struct UnitVariantAccess<'a, R: 'a, F: 'a> {
  de: &'a mut Deserializer<R, F>,
}

impl<'a, R: 'a, F: 'a> UnitVariantAccess<'a, R, F> {
  fn new(de: &'a mut Deserializer<R, F>) -> Self {
    UnitVariantAccess { de }
  }
}

impl<'de, 'a, R: Read<'de> + 'a, F: ReadFormatter<'de> + 'a> de::EnumAccess<'de>
  for UnitVariantAccess<'a, R, F>
{
  type Error = Error;
  type Variant = Self;

  fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self)>
  where
    V: de::DeserializeSeed<'de>,
  {
    let variant = e!(seed.deserialize(&mut *self.de));
    Ok((variant, self))
  }
}

impl<'de, 'a, R: Read<'de> + 'a, F: ReadFormatter<'de> + 'a> de::VariantAccess<'de>
  for UnitVariantAccess<'a, R, F>
{
  type Error = Error;

  fn unit_variant(self) -> Result<()> {
    Ok(())
  }

  fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value>
  where
    T: de::DeserializeSeed<'de>,
  {
    Err(de::Error::invalid_type(
      Unexpected::UnitVariant,
      &"newtype variant",
    ))
  }

  fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    Err(de::Error::invalid_type(
      Unexpected::UnitVariant,
      &"tuple variant",
    ))
  }

  fn struct_variant<V>(
    self,
    _fields: &'static [&'static str],
    _visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    Err(de::Error::invalid_type(
      Unexpected::UnitVariant,
      &"struct variant",
    ))
  }
}

/// Only deserialize from this after peeking a '"' byte! Otherwise it may
/// deserialize invalid SXP successfully.
struct MapKey<'a, R: 'a, F: ReadFormatter<'a>> {
  de: &'a mut Deserializer<R, F>,
}

macro_rules! deserialize_numeric_key {
  ($method:ident) => {
    fn $method<V>(self, visitor: V) -> Result<V::Value>
    where
      V: de::Visitor<'de>,
    {
      self.deserialize_number(visitor)
    }
  };

  ($method:ident, $delegate:ident) => {
    fn $method<V>(self, visitor: V) -> Result<V::Value>
    where
      V: de::Visitor<'de>,
    {
      self.de.eat_char();

      match e!(self.de.peek()) {
        Some(b'0'..=b'9' | b'-') => {}
        _ => return Err(self.de.error(ErrorCode::InvalidNumber)),
      }

      let value = e!(self.de.$delegate(visitor));

      match e!(self.de.peek()) {
        Some(b'"') => self.de.eat_char(),
        _ => return Err(self.de.peek_error(ErrorCode::ExpectedDoubleQuote)),
      }

      Ok(value)
    }
  };
}

impl<'de, 'a, R, F> MapKey<'a, R, F>
where
  R: Read<'de>,
  F: ReadFormatter<'a>,
{
  deserialize_numeric_key!(deserialize_number, deserialize_number);
}

impl<'de, 'a, R, F: ReadFormatter<'a>> de::Deserializer<'de> for MapKey<'a, R, F>
where
  R: Read<'de>,
{
  type Error = Error;

  #[inline]
  fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.de.eat_char();
    self.de.scratch.clear();
    match e!(self.de.read.parse_str(&mut self.de.scratch)) {
      Reference::Borrowed(s) => visitor.visit_borrowed_str(s),
      Reference::Copied(s) => visitor.visit_str(s),
    }
  }

  deserialize_numeric_key!(deserialize_i8);
  deserialize_numeric_key!(deserialize_i16);
  deserialize_numeric_key!(deserialize_i32);
  deserialize_numeric_key!(deserialize_i64);
  deserialize_numeric_key!(deserialize_i128, deserialize_i128);
  deserialize_numeric_key!(deserialize_u8);
  deserialize_numeric_key!(deserialize_u16);
  deserialize_numeric_key!(deserialize_u32);
  deserialize_numeric_key!(deserialize_u64);
  deserialize_numeric_key!(deserialize_u128, deserialize_u128);
  deserialize_numeric_key!(deserialize_f32);
  deserialize_numeric_key!(deserialize_f64);

  #[inline]
  fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    // Map keys cannot be null.
    visitor.visit_some(self)
  }

  #[inline]
  fn deserialize_newtype_struct<V>(
    self,
    name: &'static str,
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    let _ = name;
    visitor.visit_newtype_struct(self)
  }

  #[inline]
  fn deserialize_enum<V>(
    self,
    name: &'static str,
    variants: &'static [&'static str],
    visitor: V,
  ) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.de.deserialize_enum(name, variants, visitor)
  }

  #[inline]
  fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.de.deserialize_bytes(visitor)
  }

  #[inline]
  fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
  where
    V: de::Visitor<'de>,
  {
    self.de.deserialize_bytes(visitor)
  }

  forward_to_deserialize_any! {
      bool char str string unit unit_struct seq tuple tuple_struct map struct
      identifier ignored_any
  }
}

//////////////////////////////////////////////////////////////////////////////

/// Iterator that deserializes a stream into multiple SXP values.
///
/// A stream deserializer can be created from any SXP deserializer using the
/// `Deserializer::into_iter` method.
///
/// The data can consist of any SXP value. Values need to be a self-delineating
/// value e.g. arrays, objects, or strings, or be followed by whitespace or a
/// self-delineating value.
pub struct StreamDeserializer<'de, R, F: ReadFormatter<'de>, T> {
  de: Deserializer<R, F>,
  offset: usize,
  failed: bool,
  output: PhantomData<T>,
  lifetime: PhantomData<&'de ()>,
}

impl<'de, R, F, T> StreamDeserializer<'de, R, F, T>
where
  R: read::Read<'de>,
  T: de::Deserialize<'de>,
  F: ReadFormatter<'de>,
{
  /// Create a SXP stream deserializer from one of the possible sxp
  /// input sources.
  ///
  /// Typically it is more convenient to use one of these methods instead:
  ///
  ///   - Deserializer::from_str(...).into_iter()
  ///   - Deserializer::from_slice(...).into_iter()
  ///   - Deserializer::from_reader(...).into_iter()
  pub fn new(read: R, fmt: F) -> Self {
    let offset = read.byte_offset();
    StreamDeserializer {
      de: Deserializer::new(read, fmt),
      offset,
      failed: false,
      output: PhantomData,
      lifetime: PhantomData,
    }
  }

  /// Returns the number of bytes so far deserialized into a successful `T`.
  pub fn byte_offset(&self) -> usize {
    self.offset
  }

  fn peek_end_of_value(&mut self) -> Result<()> {
    match e!(self.de.peek()) {
      Some(
        b' ' | b'\n' | b'\t' | b'\r' | b'"' | b'(' | b')' | b',' // TODO
        | b':',
      )
      | None => Ok(()),
      Some(_) => {
        let position = self.de.read.peek_position();
        Err(Error::syntax(
          ErrorCode::TrailingCharacters,
          position.line,
          position.column,
        ))
      }
    }
  }
}

impl<'de, R, F, T> Iterator for StreamDeserializer<'de, R, F, T>
where
  R: Read<'de>,
  T: de::Deserialize<'de>,
  F: ReadFormatter<'de>,
{
  type Item = Result<T>;

  fn next(&mut self) -> Option<Result<T>> {
    if R::should_early_return_if_failed && self.failed {
      return None;
    }

    // skip whitespaces, if any
    // this helps with trailing whitespaces, since whitespaces between
    // values are handled for us.
    match self.de.parse_whitespace() {
      Ok(None) => {
        self.offset = self.de.read.byte_offset();
        None
      }
      Ok(Some(b)) => {
        // If the value does not have a clear way to show the end of the value
        // (like numbers, null, true etc.) we have to look for whitespace or
        // the beginning of a self-delineated value.
        let self_delineated_value = match b {
          b'"' | b'(' => true,
          _ => false,
        };
        self.offset = self.de.read.byte_offset();
        let result = de::Deserialize::deserialize(&mut self.de);

        Some(match result {
          Ok(value) => {
            self.offset = self.de.read.byte_offset();
            if self_delineated_value {
              Ok(value)
            } else {
              self.peek_end_of_value().map(|_| value)
            }
          }
          Err(e) => {
            self.de.read.set_failed(&mut self.failed);
            Err(e)
          }
        })
      }
      Err(e) => {
        self.de.read.set_failed(&mut self.failed);
        Some(Err(e))
      }
    }
  }
}

impl<'de, R, F, T> FusedIterator for StreamDeserializer<'de, R, F, T>
where
  R: Read<'de> + Fused,
  T: de::Deserialize<'de>,
  F: ReadFormatter<'de>,
{
}

pub fn from_traits<'de, R: Read<'de>, F: ReadFormatter<'de>, T: de::Deserialize<'de>>(
  r: R,
  f: F,
) -> Result<T> {
  let mut de = Deserializer::new(r, f);
  let value = e!(de::Deserialize::deserialize(&mut de));

  // Make sure the whole stream has been consumed.
  e!(de.end());
  Ok(value)
}

pub fn from_reader<'de, R: crate::io::Read, T: de::DeserializeOwned>(
  rdr: R,
) -> Result<T> {
  from_traits(read::IoRead::new(rdr), DefaultFormatter)
}

pub fn from_str<'de, T: de::Deserialize<'de>>(v: &'de str) -> Result<T> {
  from_traits(read::StrRead::new(v), DefaultFormatter)
}

pub fn from_slice<'de, T: Deserialize<'de>>(v: &'de [u8]) -> Result<T> {
  from_traits(read::SliceRead::new(v), DefaultFormatter)
}
