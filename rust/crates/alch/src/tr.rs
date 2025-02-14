use std::{
  alloc::{GlobalAlloc, Layout, System},
  sync::atomic::{AtomicU64, Ordering},
};

/// a simple global allocator for tracing memory usage
/// from https://github.com/datafuselabs/databend/issues/1148#issuecomment-907829698
pub struct TrAlloc(System, AtomicU64, AtomicU64);

unsafe impl GlobalAlloc for TrAlloc {
  unsafe fn alloc(&self, l: Layout) -> *mut u8 {
    self.1.fetch_add(l.size() as u64, Ordering::SeqCst);
    self.2.fetch_add(l.size() as u64, Ordering::SeqCst);
    self.0.alloc(l)
  }
  unsafe fn dealloc(&self, ptr: *mut u8, l: Layout) {
    self.0.dealloc(ptr, l);
    self.1.fetch_sub(l.size() as u64, Ordering::SeqCst);
  }
}

impl TrAlloc {
  pub const fn new(s: System) -> Self {
    TrAlloc(s, AtomicU64::new(0), AtomicU64::new(0))
  }

  pub fn reset(&self) {
    self.1.store(0, Ordering::SeqCst);
    self.2.store(0, Ordering::SeqCst);
  }

  pub fn get(&self) -> u64 {
    self.1.load(Ordering::SeqCst)
  }

  pub fn get_sum(&self) -> u64 {
    self.2.load(Ordering::SeqCst)
  }
}
