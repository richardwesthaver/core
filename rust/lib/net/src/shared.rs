use std::{fmt, net::SocketAddr, time::Instant};
use bytes::{Buf, BufMut, BytesMut};
use crate::proto::coding::BufExt;

pub const MAX_CID_SIZE: usize = 20;

/// Events sent from an Endpoint to a Connection
#[derive(Debug)]
pub struct ConnectionEvent(pub(crate) ConnectionEventInner);

#[derive(Debug)]
pub(crate) enum ConnectionEventInner {
  /// A datagram has been received for the Connection
  Datagram {
    now: Instant,
    remote: SocketAddr,
    remaining: Option<BytesMut>,
  },
  /// New connection identifiers have been issued for the Connection
  NewIdentifiers(Vec<IssuedCid>),
}

/// Events sent from a Connection to an Endpoint
#[derive(Debug)]
pub struct EndpointEvent(pub(crate) EndpointEventInner);

impl EndpointEvent {
  /// Construct an event that indicating that a `Connection` will no longer
  /// emit events
  ///
  /// Useful for notifying an `Endpoint` that a `Connection` has been
  /// destroyed outside of the usual state machine flow, e.g. when being
  /// dropped by the user.
  pub fn drained() -> Self {
    Self(EndpointEventInner::Drained)
  }

  /// Determine whether this is the last event a `Connection` will emit
  ///
  /// Useful for determining when connection-related event loop state can be
  /// freed.
  pub fn is_drained(&self) -> bool {
    self.0 == EndpointEventInner::Drained
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum EndpointEventInner {
  /// The connection has been drained
  Drained,
  /// The connection needs connection identifiers
  NeedIdentifiers(u64),
  /// Stop routing connection ID for this sequence number to the connection
  /// When `bool == true`, a new connection ID will be issued to peer
  RetireConnectionId(u64, bool),
}

/// Protocol-level identifier for a connection.
///
/// Mainly useful for identifying this connection's packets on the wire with
/// tools like Wireshark.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ConnectionId {
  /// length of CID
  len: u8,
  /// CID in byte array
  bytes: [u8; MAX_CID_SIZE],
}

impl ConnectionId {
  /// Construct cid from byte array
  pub fn new(bytes: &[u8]) -> Self {
    debug_assert!(bytes.len() <= MAX_CID_SIZE);
    let mut res = Self {
      len: bytes.len() as u8,
      bytes: [0; MAX_CID_SIZE],
    };
    res.bytes[..bytes.len()].clone_from_slice(&bytes);
    res
  }

  pub(crate) fn decode(buf: &mut impl Buf) -> Option<Self> {
    let len = buf.get::<u8>().ok()? as usize;
    if len > MAX_CID_SIZE || buf.remaining() < len {
      return None;
    }
    let cid = ConnectionId::new(&buf.chunk()[..len]);
    buf.advance(len);
    Some(cid)
  }

  pub(crate) fn encode(&self, buf: &mut impl BufMut) {
    buf.put_u8(self.len() as u8);
    buf.put_slice(self);
  }
}

impl ::std::ops::Deref for ConnectionId {
  type Target = [u8];
  fn deref(&self) -> &[u8] {
    &self.bytes[0..self.len as usize]
  }
}

impl ::std::ops::DerefMut for ConnectionId {
  fn deref_mut(&mut self) -> &mut [u8] {
    &mut self.bytes[0..self.len as usize]
  }
}

impl fmt::Debug for ConnectionId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.bytes[0..self.len as usize].fmt(f)
  }
}

impl fmt::Display for ConnectionId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for byte in self.iter() {
      write!(f, "{:02x}", byte)?;
    }
    Ok(())
  }
}

#[derive(Debug, Copy, Clone)]
pub struct IssuedCid {
  pub sequence: u64,
  pub id: ConnectionId,
}
