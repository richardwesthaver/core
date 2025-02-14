//! net tests
use std::net::UdpSocket;
#[test]
fn udp_start() {
  let socket = UdpSocket::bind("127.0.0.1:0").expect("UdpSocket::bind failed");
  assert!(socket.broadcast().is_ok());
}
