//! tests/de.rs --- DE
//  TODO 2023-07-09
use serde_derive::Deserialize;
use std::io::BufReader;
// use sxp::{
//   from_reader, from_slice, from_str, from_traits,
//   read::{SliceRead, StrRead},
//   DefaultFormatter,
// };

// #[test]
// fn de_struct() {
//   #[derive(Deserialize, PartialEq, Debug)]
//   struct Test {
//     int: f32,
//     seq: Vec<String>,
//   }
//   // external tagging is serde default behavior
//   // let j = r#"(:int 1 :seq ("a" "b"))"#;
//   let j = r#"(1 ("a" "b"))"#;
//   let expected = Test {
//     int: 1.,
//     seq: vec!["a".to_owned(), "b".to_owned()],
//   };
//   assert_eq!(expected, from_str(j).unwrap());
//   assert_eq!(expected, from_slice(j.as_bytes()).unwrap());
//   assert_eq!(expected, from_reader(BufReader::new(j.as_bytes())).unwrap());
//   assert_eq!(
//     expected,
//     from_traits(SliceRead::new(j.as_bytes()), DefaultFormatter).unwrap()
//   );
//   assert_eq!(
//     expected,
//     from_traits(StrRead::new(j), DefaultFormatter).unwrap()
//   );
// }

// #[test]
// fn de_enum() {
//   #[derive(Deserialize, PartialEq, Debug)]
//   enum E {
//     Unit,
//     Newtype(u32),
//     Tuple(u32, u32),
//     Struct { a: u32 },
//   }

//   let j = r#""Unit""#;
//   let expected = E::Unit;
//   assert_eq!(expected, from_str(j).unwrap());
//   assert_eq!(expected, from_slice(j.as_bytes()).unwrap());
//   assert_eq!(expected, from_reader(BufReader::new(j.as_bytes())).unwrap());
//   assert_eq!(
//     expected,
//     from_traits(SliceRead::new(j.as_bytes()), DefaultFormatter).unwrap()
//   );
//   assert_eq!(
//     expected,
//     from_traits(StrRead::new(j), DefaultFormatter).unwrap()
//   );

//   let j = r#"("Newtype" 1)"#;
//   let expected = E::Newtype(1);
//   assert_eq!(expected, from_str(j).unwrap());
//   assert_eq!(expected, from_slice(j.as_bytes()).unwrap());
//   assert_eq!(expected, from_reader(BufReader::new(j.as_bytes())).unwrap());
//   assert_eq!(
//     expected,
//     from_traits(SliceRead::new(j.as_bytes()), DefaultFormatter).unwrap()
//   );
//   assert_eq!(
//     expected,
//     from_traits(StrRead::new(j), DefaultFormatter).unwrap()
//   );

//   let j = r#"("Tuple" (1 2))"#;
//   let expected = E::Tuple(1, 2);
//   assert_eq!(expected, from_str(j).unwrap());
//   assert_eq!(expected, from_slice(j.as_bytes()).unwrap());
//   assert_eq!(expected, from_reader(BufReader::new(j.as_bytes())).unwrap());
//   assert_eq!(
//     expected,
//     from_traits(SliceRead::new(j.as_bytes()), DefaultFormatter).unwrap()
//   );
//   assert_eq!(
//     expected,
//     from_traits(StrRead::new(j), DefaultFormatter).unwrap()
//   );

//   let j = r#"("Struct" (1))"#;
//   let expected = E::Struct { a: 1 };
//   assert_eq!(expected, from_str(j).unwrap());
//   assert_eq!(expected, from_slice(j.as_bytes()).unwrap());
//   assert_eq!(expected, from_reader(BufReader::new(j.as_bytes())).unwrap());
//   assert_eq!(
//     expected,
//     from_traits(SliceRead::new(j.as_bytes()), DefaultFormatter).unwrap()
//   );
//   assert_eq!(
//     expected,
//     from_traits(StrRead::new(j), DefaultFormatter).unwrap()
//   );
// }

// #[test]
// fn de_form() {}
