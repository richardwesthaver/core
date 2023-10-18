//! udp client example

//! to use this example - start one instance, then in a separate
//! terminal start another which connects to the first:

//! > cargo run  --example udp 9980

//! > cargo run --example udp 9981 9980

//! PS - you can change the formatter by specifying one on the command
//! line. It affects both read and write streams.

//! > cargo run --example udp 9980 binary
use serde_derive::{Deserialize, Serialize};
use std::net::SocketAddr;
use std::net::UdpSocket;
use std::time::Duration;
#[derive(Serialize, Deserialize)]
struct Packet {
  id: usize,
  key: String,
  payload: Vec<f64>,
}

impl Packet {
  fn new() -> Self {
    Packet {
      id: 0,
      key: String::new(),
      payload: vec![],
    }
  }
  fn next(&mut self) -> Vec<u8> {
    self.id += 1;
    self.payload = rand::random::<[f64; 32]>().to_vec();
    sxp::to_vec(&self).unwrap()
  }
}

fn main() {
  let mut args = std::env::args().skip(1);
  let addr = SocketAddr::new(
    "127.0.0.1".parse().unwrap(),
    args.next().unwrap().parse::<u16>().unwrap(),
  );
  let socket = UdpSocket::bind(addr).expect("failed to bind socket");
  socket
    .set_read_timeout(Some(Duration::from_millis(500)))
    .unwrap();
  let mut peer = None;
  let mut rx = [0u8; 1024];
  let mut tx = Packet::new();
  while let Some(x) = args.next() {
    if let Ok(x) = x.parse::<u16>() {
      peer = Some(SocketAddr::new("127.0.0.1".parse().unwrap(), x));
      socket.connect(peer.unwrap()).unwrap();
    } else {
      match x.as_str() {
        "default" => (),
        "binary" => (),
        "pretty" => (),
        _ => eprintln!("invalid formatter"),
      }
    }
  }

  loop {
    if let Ok(_) = socket.recv(&mut rx) {
      println!("{}", String::from_utf8_lossy(&rx).trim_end_matches('\0'));
      rx = rx.map(|_| 0);
    } else if let Some(p) = peer {
      std::thread::sleep(Duration::from_millis(500));
      tx.key = String::from_utf8_lossy(rand::random::<[u8; 32]>().as_slice())
        .to_string();
      // send a struct
      let bytes = socket.send(&mut tx.next()).unwrap();
      println!("(:sent {bytes} :to {p})");
    }
  }
}
