//! # coll
use crate::Objective;

use hash::Id;
use std::collections::BTreeMap;

/// Collection container trait for single-typed sets
pub type Coll<T> = Vec<Box<Te>>;

pub struct Collection<T: Objective>(BTreeMap<Id, T>);
