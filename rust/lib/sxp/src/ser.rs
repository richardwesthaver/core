//! ser.rs --- SXP Serializer
use crate::fmt::{DefaultFormatter, WriteFormatter};
use crate::tok::{format_escaped_str, format_escaped_str_contents};
use crate::{err::ErrorCode, Error, Result};
use core::fmt::{self, Display};
use core::num::FpCategory;
use serde::ser::{self, Impossible, Serialize};
use std::io::{self, Write};

#[inline]
pub fn to_writer<W: Write, S: ?Sized + Serialize>(
  writer: W,
  value: &S,
) -> Result<()> {
  let mut ser = Serializer::new(writer);
  value.serialize(&mut ser)
}

/// Serialize the given data structure as a canonical SXP byte vector.
#[inline]
pub fn to_vec<S: ?Sized + Serialize>(value: &S) -> Result<Vec<u8>> {
  let mut writer = Vec::with_capacity(128);
  e!(to_writer(&mut writer, value));
  Ok(writer)
}

#[inline]
pub fn to_string<S: ?Sized + Serialize>(value: &S) -> Result<String> {
  let mut writer = Vec::new(); // TODO: with-capacity 128? bench
  to_writer(&mut writer, value)?;
  // We then check that the serialized string is the same as what we expect
  let string = unsafe { String::from_utf8_unchecked(writer) };
  Ok(string)
}

pub struct Serializer<W, F = DefaultFormatter> {
  writer: W,
  formatter: F,
}

impl<W: Write> Serializer<W> {
  #[inline]
  fn new(writer: W) -> Self {
    Serializer::with_formatter(writer, DefaultFormatter)
  }
}

impl<W: Write, F: WriteFormatter> Serializer<W, F> {
  fn with_formatter(writer: W, formatter: F) -> Self {
    Serializer { writer, formatter }
  }
  #[inline]
  pub fn into_inner(self) -> W {
    self.writer
  }
  #[inline]
  pub fn serialize_symbol(&mut self, value: &str) -> Result<()> {
    self
      .formatter
      .write_symbol(&mut self.writer, value)
      .map_err(Error::io)
  }
}

// impl<'a, W:Write> Serializer<W, PrettyFormatter<'a>> {...}

impl<'ser, W: Write, F: WriteFormatter> serde::ser::Serializer
  for &'ser mut Serializer<W, F>
{
  type Ok = (); // could be usize for bytes written
  type Error = Error;
  type SerializeSeq = Seq<'ser, W, F>;
  type SerializeTuple = Seq<'ser, W, F>;
  type SerializeTupleStruct = Seq<'ser, W, F>;
  type SerializeTupleVariant = Seq<'ser, W, F>;
  type SerializeMap = Seq<'ser, W, F>;
  type SerializeStruct = Seq<'ser, W, F>;
  type SerializeStructVariant = Seq<'ser, W, F>;
  // TODO
  fn serialize_bool(self, v: bool) -> Result<()> {
    self
      .formatter
      .write_bool(&mut self.writer, v)
      .map_err(Error::io)
  }
  #[inline]
  fn serialize_i8(self, value: i8) -> Result<()> {
    self
      .formatter
      .write_i8(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_i16(self, value: i16) -> Result<()> {
    self
      .formatter
      .write_i16(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_i32(self, value: i32) -> Result<()> {
    self
      .formatter
      .write_i32(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_i64(self, value: i64) -> Result<()> {
    self
      .formatter
      .write_i64(&mut self.writer, value)
      .map_err(Error::io)
  }

  fn serialize_i128(self, value: i128) -> Result<()> {
    self
      .formatter
      .write_i128(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_u8(self, value: u8) -> Result<()> {
    self
      .formatter
      .write_u8(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_u16(self, value: u16) -> Result<()> {
    self
      .formatter
      .write_u16(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_u32(self, value: u32) -> Result<()> {
    self
      .formatter
      .write_u32(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_u64(self, value: u64) -> Result<()> {
    self
      .formatter
      .write_u64(&mut self.writer, value)
      .map_err(Error::io)
  }

  fn serialize_u128(self, value: u128) -> Result<()> {
    self
      .formatter
      .write_u128(&mut self.writer, value)
      .map_err(Error::io)
  }

  #[inline]
  fn serialize_f32(self, value: f32) -> Result<()> {
    match value.classify() {
      // TODO
      FpCategory::Nan | FpCategory::Infinite => self
        .formatter
        .write_nil(&mut self.writer)
        .map_err(Error::io),
      _ => self
        .formatter
        .write_f32(&mut self.writer, value)
        .map_err(Error::io),
    }
  }
  #[inline]
  fn serialize_f64(self, value: f64) -> Result<()> {
    match value.classify() {
      FpCategory::Nan | FpCategory::Infinite => self
        .formatter
        .write_nil(&mut self.writer)
        .map_err(Error::io),
      _ => self
        .formatter
        .write_f64(&mut self.writer, value)
        .map_err(Error::io),
    }
  }
  #[inline]
  fn serialize_char(self, value: char) -> Result<()> {
    // A char encoded as UTF-8 takes 4 bytes at most.
    let mut buf = [0; 4];
    self.serialize_str(value.encode_utf8(&mut buf))
  }
  #[inline]
  fn serialize_str(self, value: &str) -> Result<()> {
    format_escaped_str(&mut self.writer, &mut self.formatter, value)
      .map_err(Error::io)
  }
  #[inline]
  fn serialize_bytes(self, value: &[u8]) -> Result<()> {
    use serde::ser::SerializeSeq;
    let mut seq = e!(self.serialize_seq(Some(value.len())));
    for byte in value {
      e!(seq.serialize_element(byte));
    }
    seq.end()
  }
  #[inline]
  fn serialize_unit(self) -> Result<()> {
    self
      .formatter
      .write_nil(&mut self.writer)
      .map_err(Error::io)
  }
  #[inline]
  fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
    self.serialize_unit()
  }

  #[inline]
  fn serialize_unit_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
  ) -> Result<()> {
    self.serialize_symbol(variant)
  }

  /// Serialize newtypes without an object wrapper.
  #[inline]
  fn serialize_newtype_struct<T>(
    self,
    _name: &'static str,
    value: &T,
  ) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    value.serialize(self)
  }

  #[inline]
  fn serialize_newtype_variant<T>(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    value: &T,
  ) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    e!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(Error::io));
    e!(self.serialize_symbol(variant));
    // e!(self
    //     .formatter
    //     .end_object_key(&mut self.writer)
    //     .map_err(|e| e.into()));
    // e!(self
    //     .formatter
    //     .begin_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    e!(self
      .formatter
      .begin_list_element(&mut self.writer, false)
      .map_err(Error::io));
    e!(value.serialize(&mut *self));
    // e!(self
    //     .formatter
    //     .end_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    self.formatter.end_list(&mut self.writer).map_err(Error::io)
  }

  #[inline]
  fn serialize_none(self) -> Result<()> {
    self.serialize_unit()
  }

  #[inline]
  fn serialize_some<T>(self, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    value.serialize(self)
  }

  #[inline]
  fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
    // TODO: impl nil for seq of 0 length?
    e!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(Error::io));
    if len == Some(0) {
      e!(self.formatter.end_list(&mut self.writer).map_err(Error::io));
      Ok(Seq {
        ser: self,
        state: State::Nil,
      })
    } else {
      Ok(Seq {
        ser: self,
        state: State::Car,
      })
    }
  }

  #[inline]
  fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
    self.serialize_seq(Some(len))
  }

  #[inline]
  fn serialize_tuple_struct(
    self,
    _name: &'static str,
    len: usize,
  ) -> Result<Self::SerializeTupleStruct> {
    self.serialize_seq(Some(len))
  }

  #[inline]
  fn serialize_tuple_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    len: usize,
  ) -> Result<Self::SerializeTupleVariant> {
    e!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(Error::io));
    e!(self.serialize_symbol(variant));
    // e!(self
    //     .formatter
    //     .end_object_key(&mut self.writer)
    //     .map_err(|e| e.into()));
    // e!(self
    //     .formatter
    //     .begin_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    e!(self
      .formatter
      .begin_list_element(&mut self.writer, false)
      .map_err(Error::io));
    self.serialize_seq(Some(len))
  }

  #[inline]
  fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
    e!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(Error::io));
    if len == Some(0) {
      e!(self.formatter.end_list(&mut self.writer).map_err(Error::io));
      Ok(Seq {
        ser: self,
        state: State::Nil,
      })
    } else {
      Ok(Seq {
        ser: self,
        state: State::Car,
      })
    }
  }

  // TODO: include struct name?
  #[inline]
  fn serialize_struct(
    self,
    _name: &'static str,
    len: usize,
  ) -> Result<Self::SerializeStruct> {
    self.serialize_map(Some(len))
  }

  #[inline]
  fn serialize_struct_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    len: usize,
  ) -> Result<Self::SerializeStructVariant> {
    e!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(Error::io));
    e!(self.serialize_symbol(variant));
    e!(self
      .formatter
      .begin_list_element(&mut self.writer, false)
      .map_err(Error::io));
    // e!(self
    //     .formatter
    //     .end_object_key(&mut self.writer)
    //     .map_err(|e| e.into()));
    // e!(self
    //     .formatter
    //     .begin_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    self.serialize_map(Some(len))
  }

  fn collect_str<T>(self, value: &T) -> Result<()>
  where
    T: ?Sized + Display,
  {
    use self::fmt::Write;

    struct Adapter<'ser, W: 'ser, F: 'ser> {
      writer: &'ser mut W,
      formatter: &'ser mut F,
      error: Option<io::Error>,
    }

    impl<'ser, W, F> Write for Adapter<'ser, W, F>
    where
      W: io::Write,
      F: WriteFormatter,
    {
      fn write_str(&mut self, s: &str) -> fmt::Result {
        debug_assert!(self.error.is_none());
        match format_escaped_str_contents(self.writer, self.formatter, s) {
          Ok(()) => Ok(()),
          Err(err) => {
            self.error = Some(err);
            Err(fmt::Error)
          }
        }
      }
    }

    e!(self
      .formatter
      .begin_string(&mut self.writer)
      .map_err(Error::io));
    {
      let mut adapter = Adapter {
        writer: &mut self.writer,
        formatter: &mut self.formatter,
        error: None,
      };
      match write!(adapter, "{}", value) {
        Ok(()) => debug_assert!(adapter.error.is_none()),
        Err(_) => {
          return Err(Error::io(
            adapter.error.expect("there should be an error"),
          ));
        }
      }
    }
    self
      .formatter
      .end_string(&mut self.writer)
      .map_err(Error::io)
  }
}

// Not public API. Should be pub(crate).
#[doc(hidden)]
#[derive(Eq, PartialEq)]
pub enum State {
  Nil,
  Car,
  Cdr,
}

pub struct Seq<'a, W: 'a, F: 'a> {
  ser: &'a mut Serializer<W, F>,
  state: State,
}

impl<'ser, W: Write, F: WriteFormatter> ser::SerializeSeq for Seq<'ser, W, F> {
  type Ok = ();
  type Error = Error;
  #[inline]
  fn serialize_element<T: ?Sized + Serialize>(
    &mut self,
    value: &T,
  ) -> Result<()> {
    let ser = &mut self.ser;
    e!(ser
      .formatter
      .begin_list_element(&mut ser.writer, self.state == State::Car)
      .map_err(Error::io));
    self.state = State::Cdr;
    e!(value.serialize(&mut **ser));
    ser
      .formatter
      .end_list_element(&mut ser.writer)
      .map_err(Error::io)
  }
  #[inline]
  fn end(self) -> Result<()> {
    match self.state {
      State::Nil => Ok(()),
      _ => self
        .ser
        .formatter
        .end_list(&mut self.ser.writer)
        .map_err(Error::io),
    }
  }
}

impl<'ser, W: Write, F: WriteFormatter> ser::SerializeTuple
  for Seq<'ser, W, F>
{
  type Ok = ();
  type Error = Error;
  #[inline]
  fn serialize_element<S: ?Sized + Serialize>(
    &mut self,
    value: &S,
  ) -> Result<()> {
    ser::SerializeSeq::serialize_element(self, value)
  }
  #[inline]
  fn end(self) -> Result<()> {
    ser::SerializeSeq::end(self)
  }
}

impl<'a, W, F> ser::SerializeTupleStruct for Seq<'a, W, F>
where
  W: io::Write,
  F: WriteFormatter,
{
  type Ok = ();
  type Error = Error;

  #[inline]
  fn serialize_field<T>(&mut self, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    ser::SerializeSeq::serialize_element(self, value)
  }

  #[inline]
  fn end(self) -> Result<()> {
    match self.state {
      State::Nil => Ok(()),
      _ => self
        .ser
        .formatter
        .end_list(&mut self.ser.writer)
        .map_err(Error::io),
    }?;
    self
      .ser
      .formatter
      .end_list(&mut self.ser.writer)
      .map_err(Error::io)
  }
}

impl<'a, W, F> ser::SerializeTupleVariant for Seq<'a, W, F>
where
  W: io::Write,
  F: WriteFormatter,
{
  type Ok = ();
  type Error = Error;

  #[inline]
  fn serialize_field<T>(&mut self, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    ser::SerializeSeq::serialize_element(self, value)
  }

  //  TODO 2023-07-09: this don't feel right
  #[inline]
  fn end(self) -> Result<()> {
    e!(self
      .ser
      .formatter
      .end_list(&mut self.ser.writer)
      .map_err(Error::io));
    ser::SerializeSeq::end(self)
  }
}

impl<'a, W, F> ser::SerializeMap for Seq<'a, W, F>
where
  W: io::Write,
  F: WriteFormatter,
{
  type Ok = ();
  type Error = Error;

  #[inline]
  fn serialize_key<T>(&mut self, key: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    let ser = &mut self.ser;
    e!(ser
      .formatter
      .begin_list_element(&mut ser.writer, self.state == State::Car)
      .map_err(Error::io));
    self.state = State::Cdr;
    e!(ser.formatter.begin_key(&mut ser.writer).map_err(Error::io));
    key.serialize(SymbolSerializer(*ser))
  }

  #[inline]
  fn serialize_value<T>(&mut self, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    let ser = &mut self.ser;
    e!(ser
      .formatter
      .begin_list_element(&mut ser.writer, false)
      .map_err(Error::io));
    e!(value.serialize(&mut **ser));
    ser
      .formatter
      .end_list_element(&mut ser.writer)
      .map_err(Error::io)
  }

  #[inline]
  fn end(self) -> Result<()> {
    self
      .ser
      .formatter
      .end_list(&mut self.ser.writer)
      .map_err(Error::io)
  }
}

impl<'a, W, F> ser::SerializeStruct for Seq<'a, W, F>
where
  W: io::Write,
  F: WriteFormatter,
{
  type Ok = ();
  type Error = Error;

  #[inline]
  fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    ser::SerializeMap::serialize_entry(self, key, value)
  }

  #[inline]
  fn end(self) -> Result<()> {
    ser::SerializeMap::end(self)
  }
}

impl<'a, W, F> ser::SerializeStructVariant for Seq<'a, W, F>
where
  W: io::Write,
  F: WriteFormatter,
{
  type Ok = ();
  type Error = Error;

  #[inline]
  fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    e!(self
      .ser
      .formatter
      .begin_key(&mut self.ser.writer)
      .map_err(Error::io));
    e!(self.ser.serialize_symbol(key));
    e!(self
      .ser
      .formatter
      .begin_list_element(&mut self.ser.writer, false)
      .map_err(Error::io));
    value.serialize(&mut *self.ser)
    //ser::SerializeMap::serialize_entry(self, key, value)
  }
  #[inline]
  fn end(self) -> Result<()> {
    match self.state {
      State::Nil => (),
      _ => e!(self
        .ser
        .formatter
        .end_list(&mut self.ser.writer)
        .map_err(Error::io)),
    };
    self
      .ser
      .formatter
      .end_list(&mut self.ser.writer)
      .map_err(Error::io)
    // ser::SerializeMap::end(self)
  }
}

fn key_sym_error() -> Error {
  Error::syntax(ErrorCode::KeyMustBeASymbol, 0, 0)
}

struct SymbolSerializer<'a, W: 'a, F: 'a>(&'a mut Serializer<W, F>);

impl<'a, W, F> ser::Serializer for SymbolSerializer<'a, W, F>
where
  W: io::Write,
  F: WriteFormatter,
{
  type Ok = ();
  type Error = Error;

  #[inline]
  fn serialize_str(self, value: &str) -> Result<()> {
    self.0.serialize_symbol(value)
  }
  #[inline]
  fn serialize_unit_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
  ) -> Result<()> {
    self.0.serialize_str(variant)
  }

  #[inline]
  fn serialize_newtype_struct<T>(
    self,
    _name: &'static str,
    value: &T,
  ) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    value.serialize(self)
  }

  type SerializeSeq = Impossible<(), Error>;
  type SerializeTuple = Impossible<(), Error>;
  type SerializeTupleStruct = Impossible<(), Error>;
  type SerializeTupleVariant = Impossible<(), Error>;
  type SerializeMap = Impossible<(), Error>;
  type SerializeStruct = Impossible<(), Error>;
  type SerializeStructVariant = Impossible<(), Error>;

  fn serialize_bool(self, _value: bool) -> Result<()> {
    Err(key_sym_error())
  }

  fn serialize_i8(self, value: i8) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_i8(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_i16(self, value: i16) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_i16(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_i32(self, value: i32) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_i32(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_i64(self, value: i64) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_i64(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_i128(self, value: i128) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_i128(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_u8(self, value: u8) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_u8(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_u16(self, value: u16) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_u16(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_u32(self, value: u32) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_u32(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_u64(self, value: u64) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_u64(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_u128(self, value: u128) -> Result<()> {
    e!(self
      .0
      .formatter
      .begin_string(&mut self.0.writer)
      .map_err(Error::io));
    e!(self
      .0
      .formatter
      .write_u128(&mut self.0.writer, value)
      .map_err(Error::io));
    self
      .0
      .formatter
      .end_string(&mut self.0.writer)
      .map_err(Error::io)
  }

  fn serialize_f32(self, _value: f32) -> Result<()> {
    Err(key_sym_error())
  }

  fn serialize_f64(self, _value: f64) -> Result<()> {
    Err(key_sym_error())
  }

  fn serialize_char(self, value: char) -> Result<()> {
    self.0.serialize_str(&value.to_string())
  }

  fn serialize_bytes(self, _value: &[u8]) -> Result<()> {
    Err(key_sym_error())
  }

  fn serialize_unit(self) -> Result<()> {
    Err(key_sym_error())
  }

  fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
    Err(key_sym_error())
  }

  fn serialize_newtype_variant<T>(
    self,
    _name: &'static str,
    _variant_index: u32,
    _variant: &'static str,
    _value: &T,
  ) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    Err(key_sym_error())
  }

  fn serialize_none(self) -> Result<()> {
    Err(key_sym_error())
  }

  fn serialize_some<T>(self, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    value.serialize(self)
  }

  fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
    Err(key_sym_error())
  }

  fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
    Err(key_sym_error())
  }

  fn serialize_tuple_struct(
    self,
    _name: &'static str,
    _len: usize,
  ) -> Result<Self::SerializeTupleStruct> {
    Err(key_sym_error())
  }

  fn serialize_tuple_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    _variant: &'static str,
    _len: usize,
  ) -> Result<Self::SerializeTupleVariant> {
    Err(key_sym_error())
  }

  fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
    Err(key_sym_error())
  }

  fn serialize_struct(
    self,
    _name: &'static str,
    _len: usize,
  ) -> Result<Self::SerializeStruct> {
    Err(key_sym_error())
  }

  fn serialize_struct_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    _variant: &'static str,
    _len: usize,
  ) -> Result<Self::SerializeStructVariant> {
    Err(key_sym_error())
  }

  fn collect_str<T>(self, value: &T) -> Result<()>
  where
    T: ?Sized + Display,
  {
    self.0.collect_str(value)
  }
}
