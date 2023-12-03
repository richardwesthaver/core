use flate::{pack, unpack, unpack_replace};
use std::{env, path::Path};
fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() == 1 {
    println!("mtz [pack|unpack|unpackr] PATH");
  } else {
    let mut args = args.iter().skip(1);
    match args.next().unwrap().as_str() {
      "pack" => {
        let path = args.next().ok_or("expected a path").unwrap();
        let path = Path::new(&path);
        pack(&path, &path.with_extension("tar.zst").as_path(), None);
      }
      "unpack" => {
        let path = args.next().ok_or("expected a path").unwrap();
        let path = Path::new(&path);
        if !&path.exists() {
          println!("file does not exist");
        } else {
          unpack(&path, &Path::new("."));
        }
      }
      "unpackd" => {
        let path = args.next().ok_or("expected a path").unwrap();
        let path = Path::new(&path);
        if !&path.exists() {
          println!("file does not exist");
        } else {
          unpack_replace(&path, &Path::new("."));
        }
      }
      _ => {
        println!("mtz [pack|unpack|unpackr] PATH");
      }
    }
  }
}
