IO library

first-class backend is IO_URING. everything else defers to whatever is
most portable (on Windows and Mac). The URING system provides low
level bindings to IO_URING.

We implement a high-level API for IO. The API is meant to be as simple
as possible, while still providing a great deal of control over the
primitive IO queues.

Speed is the priority.

- We will probably need a specialized vector class mapping/displaced directly to URING::IOVECs
- io-stream doesn't share or inherit structure from sb-sys:fd-stream
- io-socket doesn't share or inherit structure from sb-bsd-sockets:socket
- prioritizing UDP/datagram/framed sockets over TCP/streamed
- testing:
  - use NET/CODEC/TLV:TLV for passing simple messages from SQ->CQ
  - use IO-OP-NOP for estimating throughput and comparing implementations (rust/tokio)
