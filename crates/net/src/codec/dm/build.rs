//! builder for dm protocol prefab
use tokio_util::codec::{Framed, FramedRead, FramedWrite};
use super::{DecodeState, DmCodec};
use std::cmp;

use tokio::io::{AsyncRead, AsyncWrite};

// use bytes::{Buf, BufMut, Bytes, BytesMut};

#[derive(Debug, Clone, Copy)]
pub struct Builder {
    // Maximum frame length
    max_frame_len: usize,
    // Number of bytes representing the field length
    length_field_len: usize,
    // Number of bytes in the header before the length field
    length_field_offset: usize,
    // Adjust the length specified in the header field by this amount
    length_adjustment: isize,
    // Total number of bytes to skip before reading the payload, if not set,
    // `length_field_len + length_field_offset`
    num_skip: Option<usize>,
    // Length field byte order (little or big endian)
    length_field_is_big_endian: bool,
}

impl Builder {
    pub fn new() -> Self {
        Builder {
            // Default max frame length of 4KB
            max_frame_len: 4 * 1_024,
            // Default byte length of 4
            length_field_len: 4,
            // Default to the header field being at the start of the header.
            length_field_offset: 0,
            length_adjustment: 0,
            // Total number of bytes to skip before reading the payload, if not set,
            // `length_field_len + length_field_offset`
            num_skip: None,
            // Default to reading the length field in network (big) endian.
            length_field_is_big_endian: true,            
        }
    }

    pub fn big_endian(&mut self) -> &mut Self {
        self.length_field_is_big_endian = true;
        self
    }

    pub fn native_endian(&mut self) -> &mut Self {
        if cfg!(target_endian = "big") {
            self.big_endian()
        } else {
            self.native_endian()
        }
    }    

    pub fn max_frame_length(&mut self, val: usize) -> &mut Self {
        self.max_frame_len = val;
        self
    }    

    pub fn length_field_length(&mut self, val: usize) -> &mut Self {
        assert!(val > 0 && val <= 8, "invalid length field length");
        self.length_field_len = val;
        self
    }

    pub fn length_field_offset(&mut self, val: usize) -> &mut Self {
        self.length_field_offset = val;
        self
    }

    pub fn length_adjustment(&mut self, val: isize) -> &mut Self {
        self.length_adjustment = val;
        self
    }

    pub fn num_skip(&mut self, val: usize) -> &mut Self {
        self.num_skip = Some(val);
        self
    }

    pub fn new_codec(&self) -> DmCodec {
        DmCodec {
            builder: *self,
            state: DecodeState::Head,
        }
    }

    pub fn new_read<T>(&self, upstream: T) -> FramedRead<T, DmCodec>
    where
        T: AsyncRead,
    {
        FramedRead::new(upstream, self.new_codec())
    }

    pub fn new_write<T>(&self, inner: T) -> FramedWrite<T, DmCodec>
    where
        T: AsyncWrite,
    {
        FramedWrite::new(inner, self.new_codec())
    }

    pub fn new_framed<T>(&self, inner: T) -> Framed<T, DmCodec>
    where
        T: AsyncRead + AsyncWrite,
    {
        Framed::new(inner, self.new_codec())
    }

    fn num_head_bytes(&self) -> usize {
        let num = self.length_field_offset + self.length_field_len;
        cmp::max(num, self.num_skip.unwrap_or(0))
    }

    fn get_num_skip(&self) -> usize {
        self.num_skip
            .unwrap_or(self.length_field_offset + self.length_field_len)
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
