/// tests/ser.rs --- SER
use serde_derive::Serialize;
use std::io::BufWriter;
use sxp::{to_string, to_vec, to_writer};
#[test]
fn ser_struct() {
  #[derive(Serialize)]
  struct Test {
    int: u32,
    seq: Vec<&'static str>,
  }

  let test = Test {
    int: 1,
    seq: vec!["a", "b"],
  };
  let expected = r#"(:int 1 :seq ("a" "b"))"#;
  assert_eq!(to_string(&test).unwrap(), expected);
  let mut buf = vec![];
  to_writer(BufWriter::new(&mut buf), &test).unwrap();
  assert_eq!(std::str::from_utf8(&buf).unwrap(), expected);
  assert_eq!(
    std::str::from_utf8(&to_vec(&test).unwrap()).unwrap(),
    expected
  );
}

#[test]
fn ser_enum() {
  #[derive(Serialize)]
  enum E {
    Unit,
    #[serde(rename = "yoyo")]
    Yoyo,
    Newtype(u32),
    Tuple(u32, u32),
    Struct {
      a: u32,
    },
  }

  let u = E::Unit;
  let expected = r#"Unit"#;
  assert_eq!(to_string(&u).unwrap(), expected);
  let y = E::Yoyo;
  let expected = r#"yoyo"#;
  assert_eq!(to_string(&y).unwrap(), expected);
  let n = E::Newtype(1);
  let expected = r#"(Newtype 1)"#;
  assert_eq!(to_string(&n).unwrap(), expected);

  let t = E::Tuple(1, 2);
  let expected = r#"(Tuple (1 2))"#;
  assert_eq!(to_string(&t).unwrap(), expected);

  let s = E::Struct { a: 1 };
  let expected = r#"(Struct (:a 1))"#;
  assert_eq!(to_string(&s).unwrap(), expected);

  let mut buf = vec![];
  to_writer(BufWriter::new(&mut buf), &s).unwrap();
  assert_eq!(std::str::from_utf8(&buf).unwrap(), expected);
  assert_eq!(std::str::from_utf8(&to_vec(&s).unwrap()).unwrap(), expected);
}

//  TODO 2023-07-09
#[test]
fn ser_form() {}
