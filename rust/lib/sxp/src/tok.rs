//! tok.rs --- SXP Tokens
/// all possible Tokens. Note that we do not explicitly declare a
/// 'nil' variant. The modern rules of S-expressions are implicit in
/// the SXP specification.
use crate::{fmt::WriteFormatter, Error};
use serde::de::{self, Unexpected, Visitor};
use serde::{
  forward_to_deserialize_any, Deserialize, Deserializer, Serialize, Serializer,
};
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::io::{self, Write};
use std::str::FromStr;

/// Token types collected from the output of a type which implements
/// Read+Format.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
  ListStart,
  ListEnd,
  Sym(String),
  Str(String),
  Num(String),
}

impl Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Token::ListStart => f.write_str("( "),
      Token::ListEnd => f.write_str(")\n"),
      Token::Sym(s) => f.write_str(&format!("{} ", &s)),
      Token::Str(s) => f.write_str(&format!("\"{}\" ", &s)),
      Token::Num(n) => Display::fmt(&n, f),
    }
  }
}

impl FromStr for Token {
  type Err = Error;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "(" => Ok(Token::ListStart),
      ")" => Ok(Token::ListEnd),
      s => {
        if s.starts_with("\"") {
          Ok(Token::Str(s.trim_matches('"').to_owned()))
        } else if s.chars().next().unwrap().is_numeric() {
          Ok(Token::Num(s.to_owned()))
        } else {
          Ok(Token::Sym(s.to_owned()))
        }
      }
    }
  }
}

/// Potential Number
#[derive(Copy, Clone)]
pub enum PotNum {
  PosInt(u64),
  NegInt(i64),
  Float(f64),
  // Complex,Rational,
}

impl PartialEq for PotNum {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::PosInt(a), Self::PosInt(b)) => a == b,
      (Self::NegInt(a), Self::NegInt(b)) => a == b,
      (Self::Float(a), Self::Float(b)) => a == b,
      _ => false,
    }
  }
}

impl Eq for PotNum {}

impl Hash for PotNum {
  fn hash<H: Hasher>(&self, h: &mut H) {
    match *self {
      PotNum::PosInt(i) => i.hash(h),
      PotNum::NegInt(i) => i.hash(h),
      PotNum::Float(f) => {
        if f == 0.0f64 {
          // use same hash for +/-0.0
          0.0f64.to_bits().hash(h);
        } else {
          f.to_bits().hash(h);
        }
      }
    }
  }
}

/// String
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Str(std::string::String);
/// Symbol
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Sym(std::string::String);
/// Number
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Num(PotNum);

impl Num {
  /// Returns true if the `Number` is an integer between `i64::MIN` and
  /// `i64::MAX`.
  ///
  /// For any Number on which `is_i64` returns true, `as_i64` is guaranteed to
  /// return the integer value.
  #[inline]
  pub fn is_i64(&self) -> bool {
    // #[cfg(not(feature = "arbitrary_precision"))]
    match self.0 {
      PotNum::PosInt(v) => v <= i64::max_value() as u64,
      PotNum::NegInt(_) => true,
      PotNum::Float(_) => false,
    }
    // #[cfg(feature = "arbitrary_precision")]
    // self.as_i64().is_some()
  }

  /// Returns true if the `Number` is an integer between zero and `u64::MAX`.
  ///
  /// For any Number on which `is_u64` returns true, `as_u64` is guaranteed to
  /// return the integer value.
  #[inline]
  pub fn is_u64(&self) -> bool {
    // #[cfg(not(feature = "arbitrary_precision"))]
    match self.0 {
      PotNum::PosInt(_) => true,
      PotNum::NegInt(_) | PotNum::Float(_) => false,
    }
    // #[cfg(feature = "arbitrary_precision")]
    // self.as_u64().is_some()
  }

  /// Returns true if the `Number` can be represented by f64.
  ///
  /// For any Number on which `is_f64` returns true, `as_f64` is guaranteed to
  /// return the floating point value.
  ///
  /// Currently this function returns true if and only if both `is_i64` and
  /// `is_u64` return false but this is not a guarantee in the future.
  #[inline]
  pub fn is_f64(&self) -> bool {
    // #[cfg(not(feature = "arbitrary_precision"))]
    match self.0 {
      PotNum::Float(_) => true,
      PotNum::PosInt(_) | PotNum::NegInt(_) => false,
    }
    // #[cfg(feature = "arbitrary_precision")]
    // {
    //     for c in self.n.chars() {
    //         if c == '.' || c == 'e' || c == 'E' {
    //             return self.n.parse::<f64>().ok().map_or(false,
    // f64::is_finite);         }
    //     }
    //     false
    // }
  }

  /// If the `Number` is an integer, represent it as i64 if possible. Returns
  /// None otherwise.
  #[inline]
  pub fn as_i64(&self) -> Option<i64> {
    // #[cfg(not(feature = "arbitrary_precision"))]
    match self.0 {
      PotNum::PosInt(n) => {
        if n <= i64::max_value() as u64 {
          Some(n as i64)
        } else {
          None
        }
      }
      PotNum::NegInt(n) => Some(n),
      PotNum::Float(_) => None,
    }
    // #[cfg(feature = "arbitrary_precision")]
    // self.n.parse().ok()
  }

  /// If the `Number` is an integer, represent it as u64 if possible. Returns
  /// None otherwise.
  #[inline]
  pub fn as_u64(&self) -> Option<u64> {
    // #[cfg(not(feature = "arbitrary_precision"))]
    match self.0 {
      PotNum::PosInt(n) => Some(n),
      PotNum::NegInt(_) | PotNum::Float(_) => None,
    }
    // #[cfg(feature = "arbitrary_precision")]
    // self.0.parse().ok()
  }

  /// Represents the number as f64 if possible. Returns None otherwise.
  #[inline]
  pub fn as_f64(&self) -> Option<f64> {
    // #[cfg(not(feature = "arbitrary_precision"))]
    match self.0 {
      PotNum::PosInt(n) => Some(n as f64),
      PotNum::NegInt(n) => Some(n as f64),
      PotNum::Float(n) => Some(n),
    }
    // #[cfg(feature = "arbitrary_precision")]
    // self.n.parse::<f64>().ok().filter(|float| float.is_finite())
  }

  /// Converts a finite `f64` to a `Number`. Infinite or NaN values are not SXP
  /// numbers.
  #[inline]
  pub fn from_f64(f: f64) -> Option<Num> {
    if f.is_finite() {
      let n = PotNum::Float(f);
      // {
      // #[cfg(not(feature = "arbitrary_precision"))]
      //#[cfg(feature = "arbitrary_precision")]
      // {
      // ryu::Buffer::new().format_finite(f).to_owned()
      // }
      // };
      Some(Num(n))
    } else {
      None
    }
  }

  pub fn as_f32(&self) -> Option<f32> {
    // #[cfg(not(feature = "arbitrary_precision"))]
    match self.0 {
      PotNum::PosInt(n) => Some(n as f32),
      PotNum::NegInt(n) => Some(n as f32),
      PotNum::Float(n) => Some(n as f32),
    }
    // #[cfg(feature = "arbitrary_precision")]
    // self.n.parse::<f32>().ok().filter(|float| float.is_finite())
  }

  pub fn from_f32(f: f32) -> Option<Num> {
    if f.is_finite() {
      let n = PotNum::Float(f as f64);
      // {
      // #[cfg(not(feature = "arbitrary_precision"))]
      // #[cfg(feature = "arbitrary_precision")]
      // {
      // ryu::Buffer::new().format_finite(f).to_owned()
      // }
      //};
      Some(Num(n))
    } else {
      None
    }
  }
}

impl Display for Num {
  fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    match self.0 {
      PotNum::PosInt(u) => formatter.write_str(itoa::Buffer::new().format(u)),
      PotNum::NegInt(i) => formatter.write_str(itoa::Buffer::new().format(i)),
      PotNum::Float(f) => {
        formatter.write_str(ryu::Buffer::new().format_finite(f))
      }
    }
  }
}

impl Debug for Num {
  #[inline]
  fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    write!(formatter, "NUM({})", self)
  }
}

impl Serialize for Num {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    match self.0 {
      PotNum::PosInt(u) => serializer.serialize_u64(u),
      PotNum::NegInt(i) => serializer.serialize_i64(i),
      PotNum::Float(f) => serializer.serialize_f64(f),
    }
  }
}

impl<'de> Deserialize<'de> for Num {
  #[inline]
  fn deserialize<D: Deserializer<'de>>(
    deserializer: D,
  ) -> Result<Num, D::Error> {
    struct NumVisitor;
    impl<'de> Visitor<'de> for NumVisitor {
      type Value = Num;
      fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a SXP number")
      }
      #[inline]
      fn visit_i64<E>(self, value: i64) -> Result<Num, E> {
        Ok(value.into())
      }
      #[inline]
      fn visit_u64<E>(self, value: u64) -> Result<Num, E> {
        Ok(value.into())
      }
      #[inline]
      fn visit_f64<E: de::Error>(self, value: f64) -> Result<Num, E> {
        Num::from_f64(value)
          .ok_or_else(|| de::Error::custom("not a SXP number"))
      }
    }
    deserializer.deserialize_any(NumVisitor)
  }
}

macro_rules! deserialize_any {
    (@expand [$($num_string:tt)*]) => {
        #[inline]
        fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
        where
            V: Visitor<'de>,
        {
            match self.0 {
                PotNum::PosInt(u) => visitor.visit_u64(u),
                PotNum::NegInt(i) => visitor.visit_i64(i),
                PotNum::Float(f) => visitor.visit_f64(f),
            }
        }
    };

    (owned) => {
        deserialize_any!(@expand [n]);
    };

    (ref) => {
        deserialize_any!(@expand [n.clone()]);
    };
}

macro_rules! deserialize_number {
  ($deserialize:ident => $visit:ident) => {
    #[cfg(not(feature = "arbitrary_precision"))]
    fn $deserialize<V>(self, visitor: V) -> Result<V::Value, Error>
    where
      V: Visitor<'de>,
    {
      self.deserialize_any(visitor)
    }
  };
}

impl<'de> Deserializer<'de> for Num {
  type Error = Error;

  deserialize_any!(owned);

  deserialize_number!(deserialize_i8 => visit_i8);
  deserialize_number!(deserialize_i16 => visit_i16);
  deserialize_number!(deserialize_i32 => visit_i32);
  deserialize_number!(deserialize_i64 => visit_i64);
  deserialize_number!(deserialize_i128 => visit_i128);
  deserialize_number!(deserialize_u8 => visit_u8);
  deserialize_number!(deserialize_u16 => visit_u16);
  deserialize_number!(deserialize_u32 => visit_u32);
  deserialize_number!(deserialize_u64 => visit_u64);
  deserialize_number!(deserialize_u128 => visit_u128);
  deserialize_number!(deserialize_f32 => visit_f32);
  deserialize_number!(deserialize_f64 => visit_f64);

  forward_to_deserialize_any! {
      bool char str string bytes byte_buf option unit unit_struct
      newtype_struct seq tuple tuple_struct map struct enum identifier
      ignored_any
  }
}

impl<'de, 'a> Deserializer<'de> for &'a Num {
  type Error = Error;

  deserialize_any!(ref);

  deserialize_number!(deserialize_i8 => visit_i8);
  deserialize_number!(deserialize_i16 => visit_i16);
  deserialize_number!(deserialize_i32 => visit_i32);
  deserialize_number!(deserialize_i64 => visit_i64);
  deserialize_number!(deserialize_i128 => visit_i128);
  deserialize_number!(deserialize_u8 => visit_u8);
  deserialize_number!(deserialize_u16 => visit_u16);
  deserialize_number!(deserialize_u32 => visit_u32);
  deserialize_number!(deserialize_u64 => visit_u64);
  deserialize_number!(deserialize_u128 => visit_u128);
  deserialize_number!(deserialize_f32 => visit_f32);
  deserialize_number!(deserialize_f64 => visit_f64);

  forward_to_deserialize_any! {
      bool char str string bytes byte_buf option unit unit_struct
      newtype_struct seq tuple tuple_struct map struct enum identifier
      ignored_any
  }
}

macro_rules! impl_from_unsigned {
    (
        $($ty:ty),*
    ) => {
        $(
            impl From<$ty> for Num {
                #[inline]
                fn from(u: $ty) -> Self {
                  let n = PotNum::PosInt(u as u64);
                  Num(n)
                }
            }
        )*
    };
}

macro_rules! impl_from_signed {
    (
        $($ty:ty),*
    ) => {
        $(
            impl From<$ty> for Num {
                #[inline]
                fn from(i: $ty) -> Self {
                  let n =if i < 0 {
                    PotNum::NegInt(i as i64)
                  } else {
                    PotNum::PosInt(i as u64)
                  };
                  Num(n)
                }
            }
        )*
    };
}

impl_from_unsigned!(u8, u16, u32, u64, usize);
impl_from_signed!(i8, i16, i32, i64, isize);

impl Num {
  #[cold]
  pub fn unexpected(&self) -> Unexpected {
    match self.0 {
      PotNum::PosInt(u) => Unexpected::Unsigned(u),
      PotNum::NegInt(i) => Unexpected::Signed(i),
      PotNum::Float(f) => Unexpected::Float(f),
    }
  }
}

/// Represents a character escape code in a type-safe manner.
pub enum CharEscape {
  /// An escaped quote `"`
  Quote,
  /// An escaped reverse solidus `\`
  ReverseSolidus,
  /// An escaped solidus `/`
  Solidus,
  /// An escaped backspace character (usually escaped as `\b`)
  Backspace,
  /// An escaped form feed character (usually escaped as `\f`)
  FormFeed,
  /// An escaped line feed character (usually escaped as `\n`)
  LineFeed,
  /// An escaped carriage return character (usually escaped as `\r`)
  CarriageReturn,
  /// An escaped tab character (usually escaped as `\t`)
  Tab,
  /// An escaped ASCII plane control character (usually escaped as
  /// `\u00XX` where `XX` are two hex characters)
  AsciiControl(u8),
}

impl CharEscape {
  #[inline]
  fn from_escape_table(escape: u8, byte: u8) -> CharEscape {
    match escape {
      self::BB => CharEscape::Backspace,
      self::TT => CharEscape::Tab,
      self::NN => CharEscape::LineFeed,
      self::FF => CharEscape::FormFeed,
      self::RR => CharEscape::CarriageReturn,
      self::QU => CharEscape::Quote,
      self::BS => CharEscape::ReverseSolidus,
      self::UU => CharEscape::AsciiControl(byte),
      _ => unreachable!(),
    }
  }
}

pub fn format_escaped_str<W, F>(
  writer: &mut W,
  formatter: &mut F,
  value: &str,
) -> io::Result<()>
where
  W: ?Sized + Write,
  F: ?Sized + WriteFormatter,
{
  e!(formatter.begin_string(writer));
  e!(format_escaped_str_contents(writer, formatter, value));
  formatter.end_string(writer)
}

pub fn format_escaped_str_contents<W, F>(
  writer: &mut W,
  formatter: &mut F,
  value: &str,
) -> io::Result<()>
where
  W: ?Sized + io::Write,
  F: ?Sized + WriteFormatter,
{
  let bytes = value.as_bytes();

  let mut start = 0;

  for (i, &byte) in bytes.iter().enumerate() {
    let escape = ESCAPE[byte as usize];
    if escape == 0 {
      continue;
    }

    if start < i {
      e!(formatter.write_string_fragment(writer, &value[start..i]));
    }

    let char_escape = CharEscape::from_escape_table(escape, byte);
    e!(formatter.write_char_escape(writer, char_escape));

    start = i + 1;
  }

  if start == bytes.len() {
    return Ok(());
  }

  formatter.write_string_fragment(writer, &value[start..])
}

const BB: u8 = b'b'; // \x08
const TT: u8 = b't'; // \x09
const NN: u8 = b'n'; // \x0A
const FF: u8 = b'f'; // \x0C
const RR: u8 = b'r'; // \x0D
const QU: u8 = b'"'; // \x22
const BS: u8 = b'\\'; // \x5C
const UU: u8 = b'u'; // \x00...\x1F except the ones above
const __: u8 = 0;

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in SXP. A value of 0 means that byte i is not escaped.
static ESCAPE: [u8; 256] = [
  //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
  UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
  UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
  __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
  __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
];
