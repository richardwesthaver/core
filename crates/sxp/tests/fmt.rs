use std::io::BufWriter;
use sxp::{DefaultFormatter, WriteFormatter};

pub struct TestFormatter;
#[test]
fn canonical_fmt() {
  let mut buf = vec![];
  let mut f = DefaultFormatter;
  f.write_nil(&mut BufWriter::new(&mut buf)).unwrap();
  assert_eq!("nil", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.begin_list(&mut BufWriter::new(&mut buf)).unwrap();
  assert_eq!("(", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.end_list(&mut BufWriter::new(&mut buf)).unwrap();
  assert_eq!(")", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.begin_list_element(&mut BufWriter::new(&mut buf), false)
    .unwrap();
  assert_eq!(" ", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.begin_list_element(&mut BufWriter::new(&mut buf), true)
    .unwrap();
  assert_eq!("", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.end_list_element(&mut BufWriter::new(&mut buf)).unwrap();
  assert_eq!("", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.begin_key(&mut BufWriter::new(&mut buf)).unwrap();
  assert_eq!(":", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.begin_string(&mut BufWriter::new(&mut buf)).unwrap();
  assert_eq!("\"", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
  f.end_string(&mut BufWriter::new(&mut buf)).unwrap();
  assert_eq!("\"", String::from_utf8(buf.clone()).unwrap());
  buf.clear();
}
