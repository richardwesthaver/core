//! fmt.rs --- SXP Formatters
use crate::io::{self, Write};
use crate::read::Read;
use crate::tok::CharEscape;
use crate::Result;

pub fn indent<W: ?Sized + io::Write>(
  wr: &mut W,
  n: usize,
  s: &[u8],
) -> io::Result<()> {
  for _ in 0..n {
    e!(wr.write_all(s));
  }
  Ok(())
}

pub trait WriteFormatter {
  /// Write a 'nil' value to the specified writer.
  #[inline]
  fn write_nil<W: ?Sized + Write>(&mut self, writer: &mut W) -> io::Result<()> {
    writer.write_all(b"nil")
  }
  /// Write a bool value to the specified writer.
  #[inline]
  fn write_bool<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: bool,
  ) -> io::Result<()> {
    if value {
      writer.write_all(b"t")
    } else {
      writer.write_all(b"nil")
    }
  }
  /// Write an i8 value like '-123' to the specified writer.
  #[inline]
  fn write_i8<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: i8,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write an i16 value like '-123' to the specified writer.
  #[inline]
  fn write_i16<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: i16,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write an i32 value like `-123` to the specified writer.
  #[inline]
  fn write_i32<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: i32,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write an i64 value like `-123` to the specified writer.
  #[inline]
  fn write_i64<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: i64,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write an i128 value like `-123` to the specified writer.
  #[inline]
  fn write_i128<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: i128,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a u8 value like `123` to the specified writer.
  #[inline]
  fn write_u8<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: u8,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a u16 value like `123` to the specified writer.
  #[inline]
  fn write_u16<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: u16,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a u32 value like `123` to the specified writer.
  #[inline]
  fn write_u32<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: u32,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a u64 value like `123` to the specified writer.
  #[inline]
  fn write_u64<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: u64,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a u128 value like `123` to the specified writer.
  #[inline]
  fn write_u128<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: u128,
  ) -> io::Result<()> {
    let mut buffer = itoa::Buffer::new();
    let s = buffer.format(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a f32 value like `-31.26e+12` to the specified writer.
  #[inline]
  fn write_f32<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: f32,
  ) -> io::Result<()> {
    let mut buffer = ryu::Buffer::new();
    let s = buffer.format_finite(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a f64 value like `-31.26e+12` to the specified writer.
  #[inline]
  fn write_f64<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: f64,
  ) -> io::Result<()> {
    let mut buffer = ryu::Buffer::new();
    let s = buffer.format_finite(value);
    writer.write_all(s.as_bytes())
  }
  /// Write a number that has already been rendered to a string.
  #[inline]
  fn write_number_str<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    value: &str,
  ) -> io::Result<()> {
    writer.write_all(value.as_bytes())
  }
  /// Called before each series of `write_string_fragment` and
  /// `write_char_escape`.  Writes a `"` to the specified writer.
  #[inline]
  fn begin_string<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
  ) -> io::Result<()> {
    writer.write_all(b"\"")
  }
  /// Called after each series of `write_string_fragment` and
  /// `write_char_escape`.  Writes a `"` to the specified writer.
  #[inline]
  fn end_string<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
  ) -> io::Result<()> {
    writer.write_all(b"\"")
  }
  /// Writes a string fragment that doesn't need any escaping to the
  /// specified writer.
  #[inline]
  fn write_string_fragment<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    fragment: &str,
  ) -> io::Result<()> {
    writer.write_all(fragment.as_bytes())
  }
  /// Writes a character escape code to the specified writer.
  #[inline]
  fn write_char_escape<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    char_escape: CharEscape,
  ) -> io::Result<()> {
    use self::CharEscape::*;
    let s = match char_escape {
      Quote => b"\\\"",
      ReverseSolidus => b"\\\\",
      Solidus => b"\\/",
      Backspace => b"\\b",
      FormFeed => b"\\f",
      LineFeed => b"\\n",
      CarriageReturn => b"\\r",
      Tab => b"\\t",
      AsciiControl(byte) => {
        static HEX_DIGITS: [u8; 16] = *b"0123456789abcdef";
        let bytes = &[
          b'\\',
          b'u',
          b'0',
          b'0',
          HEX_DIGITS[(byte >> 4) as usize],
          HEX_DIGITS[(byte & 0xF) as usize],
        ];
        return writer.write_all(bytes);
      }
    };
    writer.write_all(s)
  }
  #[inline]
  fn write_symbol<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    sym: &str,
  ) -> io::Result<()> {
    writer.write_all(sym.as_bytes())
  }
  /// Called before every list.  Writes a `(` to the specified
  /// writer.
  #[inline]
  fn begin_list<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
  ) -> io::Result<()> {
    writer.write_all(b"(")
  }
  /// Called after every list.  Writes a `)` to the specified
  /// writer.
  #[inline]
  fn end_list<W: ?Sized + Write>(&mut self, writer: &mut W) -> io::Result<()> {
    writer.write_all(b")")
  }
  /// Called before every list element.  Writes a ` ` if needed to the
  /// specified writer.
  #[inline]
  fn begin_list_element<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    car: bool,
  ) -> io::Result<()> {
    if car {
      Ok(())
    } else {
      writer.write_all(b" ")
    }
  }
  /// Called after every list element.
  #[inline]
  fn end_list_element<W: ?Sized + Write>(
    &mut self,
    _writer: &mut W,
  ) -> io::Result<()> {
    Ok(())
  }
  /// Called before every slot identifier.  Writes a `:` to the
  /// specified writer.
  #[inline]
  fn begin_key<W: ?Sized + Write>(&mut self, writer: &mut W) -> io::Result<()> {
    writer.write_all(b":")
  }

  /// Writes a raw JSON fragment that doesn't need any escaping to the
  /// specified writer.
  #[inline]
  fn write_raw_fragment<W: ?Sized + Write>(
    &mut self,
    writer: &mut W,
    fragment: &str,
  ) -> io::Result<()> {
    writer.write_all(fragment.as_bytes())
  }
}

/// The ReadFormatter is the sister interface to WriteFormatter.
pub trait ReadFormatter<'r>: Read<'r> {
  #[inline]
  fn peek<R: ?Sized + Read<'r>>(
    &mut self,
    reader: &'r mut R,
  ) -> Result<Option<u8>> {
    reader.peek()
  }

  // /// Read a 'nil' value from the specified reader.
  // #[inline]
  // fn read_nil<R:?Sized+Read<'r>>(&mut self, reader: &'r R) -> io::Result<()>
  // {   reader
  // }
}

pub trait Formatter<'r>: ReadFormatter<'r> + WriteFormatter {}
impl<'r, T: ReadFormatter<'r> + WriteFormatter> Formatter<'r> for T {}

pub struct DefaultFormatter;
// impl<'r> ReadFormatter<'r> for DefaultFormatter {}
impl WriteFormatter for DefaultFormatter {}

pub struct BinaryFormatter;
// impl<'r> ReadFormatter<'r> for BinaryFormatter {}
impl WriteFormatter for BinaryFormatter {}

pub struct PrettyFormatter;
// impl<'r> ReadFormatter<'r> for PrettyFormatter {}
impl WriteFormatter for PrettyFormatter {}
