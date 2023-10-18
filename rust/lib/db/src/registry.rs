//! registry.rs --- Registry Types
use rocksdb::{ColumnFamilyDescriptor, DBCompactionStyle, Options, DB};

// use rocksdb::{WriteBatch, WriteOptions};
use crate::Result;

// use obj::id;
use std::path::{Path, PathBuf};

//  HACK 2021-10-28: replace with obj::config type
fn get_options(max_open_files: Option<i32>) -> Options {
  // https://github.com/facebook/rocksdb/wiki/RocksDB-Tuning-Guide
  let mut opts = Options::default();
  opts.create_if_missing(true);
  opts.set_compaction_style(DBCompactionStyle::Level);
  opts.set_write_buffer_size(67_108_864); // 64mb
  opts.set_max_write_buffer_number(3);
  opts.set_target_file_size_base(67_108_864); // 64mb
  opts.set_level_zero_file_num_compaction_trigger(8);
  opts.set_level_zero_slowdown_writes_trigger(17);
  opts.set_level_zero_stop_writes_trigger(24);
  opts.set_num_levels(4);
  opts.set_max_bytes_for_level_base(536_870_912); // 512mb
  opts.set_max_bytes_for_level_multiplier(8.0);

  if let Some(max_open_files) = max_open_files {
    opts.set_max_open_files(max_open_files);
  }

  opts
}

/// Registry handle
pub struct Registry {
  pub db: DB,
  pub path: PathBuf,
}

pub struct ColumnFamilies(Vec<ColumnFamilyDescriptor>);

impl ColumnFamilies {
  pub fn new() -> Self {
    ColumnFamilies(Vec::new())
  }
}
impl Registry {
  pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
    let opts = get_options(Some(1_000));
    let path = path.as_ref();
    let db = match DB::open_cf(&opts, path, vec!["reg"]) {
      Ok(db) => db,
      Err(_) => {
        let db = DB::open(&opts, path)?;
        for cf_name in vec!["reg"] {
          db.create_cf(cf_name, &opts)?;
        }
        db
      }
    };
    Ok(Registry {
      db,
      path: path.to_path_buf(),
    })
  }
  pub fn open(self) -> Result<RegistryTransaction> {
    let h = RegistryTransaction::new(self.db);
    Ok(h)
  }
}

/// A transaction that is backed by rocksdb.
#[derive(Debug)]
pub struct RegistryTransaction {
  pub db: DB,
}

impl RegistryTransaction {
  pub fn new(db: DB) -> Self {
    RegistryTransaction { db }
  }
}
