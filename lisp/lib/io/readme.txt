IO library

first-class backend is IO_URING. everything else defers to whatever is
most portable (on Windows and Mac). The URING system provides low
level bindings to IO_URING.

We implement a high-level API for IO. The API is meant to be as simple
as possible, while still providing a great deal of control over the
primitive IO queues.

Speed is the priority, above all else.
