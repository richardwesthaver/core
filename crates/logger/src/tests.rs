//! logger tests
use crate::{
  log::{debug, info, trace, warn},
  simple,
};
#[test]
fn simple_init() {
  assert!(simple().is_ok());
  info!("now this is podracing");
  debug!("this");
  warn!("is");
  trace!("podracing");
}
