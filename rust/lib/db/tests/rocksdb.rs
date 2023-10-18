use std::{
  path::{Path, PathBuf},
  thread,
  time::Duration,
};

use rocksdb::{
  BottommostLevelCompaction, CompactOptions, CuckooTableOptions, DBCompactionStyle,
  DBWithThreadMode, Error, FifoCompactOptions, IteratorMode, MultiThreaded, Options, PerfContext,
  PerfMetric, ReadOptions, UniversalCompactOptions, UniversalCompactionStopStyle, DB,
};
use tempfile::{Builder, TempDir};

/// Temporary database path which calls DB::Destroy when DBPath is dropped.
pub struct DBPath {
  #[allow(dead_code)]
  dir: TempDir, // kept for cleaning up during drop
  path: PathBuf,
}

impl DBPath {
  /// Produces a fresh (non-existent) temporary path which will be
  /// DB::destroy'ed automatically.
  pub fn new(prefix: &str) -> DBPath {
    let dir = Builder::new()
      .prefix(prefix)
      .tempdir()
      .expect("Failed to create temporary path for db.");
    let path = dir.path().join("db");

    DBPath { dir, path }
  }
}

impl Drop for DBPath {
  fn drop(&mut self) {
    let opts = Options::default();
    DB::destroy(&opts, &self.path).expect("Failed to destroy temporary DB");
  }
}

/// Convert a DBPath ref to a Path ref.
/// We don't implement this for DBPath values because we want them to
/// exist until the end of their scope, not get passed in to functions and
/// dropped early.
impl AsRef<Path> for &DBPath {
  fn as_ref(&self) -> &Path {
    &self.path
  }
}

pub fn get_byte_slice<T: AsRef<[u8]>>(source: &'_ T) -> &'_ [u8] {
  source.as_ref()
}

#[test]
fn external() {
  let path = DBPath::new("_rust_rocksdb_external");

  {
    let db = DB::open_default(&path).unwrap();

    assert!(db.put(b"k1", b"v1111").is_ok());

    let r: Result<Option<Vec<u8>>, Error> = db.get(b"k1");

    assert_eq!(r.unwrap().unwrap(), b"v1111");
    assert!(db.delete(b"k1").is_ok());
    assert!(db.get(b"k1").unwrap().is_none());
  }
}

#[test]
fn db_vector_as_ref_byte_slice() {
  let path = DBPath::new("_rust_rocksdb_db_vector_as_ref_byte_slice");

  {
    let db = DB::open_default(&path).unwrap();

    assert!(db.put(b"k1", b"v1111").is_ok());

    let result = db.get(b"k1").unwrap().unwrap();

    assert_eq!(get_byte_slice(&result), b"v1111");
  }
}

#[test]
fn iterator_upper_bound() {
  let path = DBPath::new("_rust_rocksdb_iterator_upper_bound");
  {
    let db = DB::open_default(&path).unwrap();
    db.put(b"k1", b"v1").unwrap();
    db.put(b"k2", b"v2").unwrap();
    db.put(b"k3", b"v3").unwrap();
    db.put(b"k4", b"v4").unwrap();
    db.put(b"k5", b"v5").unwrap();

    let mut readopts = ReadOptions::default();
    readopts.set_iterate_upper_bound(b"k4".to_vec());

    let iter = db.iterator_opt(IteratorMode::Start, readopts);
    let expected: Vec<_> = vec![(b"k1", b"v1"), (b"k2", b"v2"), (b"k3", b"v3")]
      .into_iter()
      .map(|(k, v)| (k.to_vec().into_boxed_slice(), v.to_vec().into_boxed_slice()))
      .collect();
    assert_eq!(expected, iter.collect::<Vec<_>>());
  }
}

#[test]
fn iterator_lower_bound() {
  let path = DBPath::new("_rust_rocksdb_iterator_lower_bound");
  {
    let db = DB::open_default(&path).unwrap();
    db.put(b"k1", b"v1").unwrap();
    db.put(b"k2", b"v2").unwrap();
    db.put(b"k3", b"v3").unwrap();
    db.put(b"k4", b"v4").unwrap();
    db.put(b"k5", b"v5").unwrap();

    let mut readopts = ReadOptions::default();
    readopts.set_iterate_lower_bound(b"k4".to_vec());

    let iter = db.iterator_opt(IteratorMode::Start, readopts);
    let expected: Vec<_> = vec![(b"k4", b"v4"), (b"k5", b"v5")]
      .into_iter()
      .map(|(k, v)| (k.to_vec().into_boxed_slice(), v.to_vec().into_boxed_slice()))
      .collect();
    assert_eq!(expected, iter.collect::<Vec<_>>());
  }
}

#[test]
fn get_updates_since_empty() {
  let path = DBPath::new("_rust_rocksdb_get_updates_since_empty");
  let db = DB::open_default(&path).unwrap();
  // get_updates_since() on an empty database
  let mut iter = db.get_updates_since(0).unwrap();
  assert!(iter.next().is_none());
}

#[test]
fn get_updates_since_out_of_range() {
  let path = DBPath::new("_rust_rocksdb_get_updates_since_out_of_range");
  let db = DB::open_default(&path).unwrap();
  db.put(b"key1", b"value1").unwrap();
  // get_updates_since() with an out of bounds sequence number
  let result = db.get_updates_since(1000);
  assert!(result.is_err());
}

#[test]
fn open_as_secondary() {
  let primary_path = DBPath::new("_rust_rocksdb_open_as_secondary_primary");

  let db = DB::open_default(&primary_path).unwrap();
  db.put(b"key1", b"value1").unwrap();

  let mut opts = Options::default();
  opts.set_max_open_files(-1);

  let secondary_path = DBPath::new("_rust_rocksdb_open_as_secondary_secondary");
  let secondary = DB::open_as_secondary(&opts, &primary_path, &secondary_path).unwrap();

  let result = secondary.get(b"key1").unwrap().unwrap();
  assert_eq!(get_byte_slice(&result), b"value1");

  db.put(b"key1", b"value2").unwrap();
  assert!(secondary.try_catch_up_with_primary().is_ok());

  let result = secondary.get(b"key1").unwrap().unwrap();
  assert_eq!(get_byte_slice(&result), b"value2");
}

#[test]
fn open_with_ttl() {
  let path = DBPath::new("_rust_rocksdb_open_with_ttl");

  let mut opts = Options::default();
  opts.create_if_missing(true);
  let db = DB::open_with_ttl(&opts, &path, Duration::from_secs(1)).unwrap();
  db.put(b"key1", b"value1").unwrap();

  thread::sleep(Duration::from_secs(2));
  // Trigger a manual compaction, this will check the TTL filter
  // in the database and drop all expired entries.
  db.compact_range(None::<&[u8]>, None::<&[u8]>);
  assert!(db.get(b"key1").unwrap().is_none());
}

#[test]
fn open_cf_with_ttl() {
  let path = DBPath::new("_rust_rocksdb_open_cf_with_ttl");

  let mut opts = Options::default();
  opts.create_if_missing(true);
  opts.create_missing_column_families(true);
  let db = DB::open_cf_with_ttl(&opts, &path, &["test_cf"], Duration::from_secs(1)).unwrap();
  let cf = db.cf_handle("test_cf").unwrap();
  db.put_cf(cf, b"key1", b"value1").unwrap();

  thread::sleep(Duration::from_secs(2));
  // Trigger a manual compaction, this will check the TTL filter
  // in the database and drop all expired entries.
  db.compact_range_cf(cf, None::<&[u8]>, None::<&[u8]>);

  assert!(db.get_cf(cf, b"key1").unwrap().is_none());
}

#[test]
fn open_with_multiple_refs_as_multi_threaded() {
  // This tests multiple references can be allowed while creating column families
  let primary_path = DBPath::new("_rust_rocksdb_open_as_multi_threaded");

  let db = DBWithThreadMode::<MultiThreaded>::open_default(&primary_path).unwrap();
  let db_ref1 = &db;
  let db_ref2 = &db;
  let opts = Options::default();
  db_ref1.create_cf("cf1", &opts).unwrap();
  db_ref2.create_cf("cf2", &opts).unwrap();
}

#[test]
fn compact_range() {
  let path = DBPath::new("_rust_rocksdb_compact_range");
  {
    let mut opts = Options::default();
    opts.create_if_missing(true);
    opts.create_missing_column_families(true);

    // set compaction style
    {
      let mut uni_co_opts = UniversalCompactOptions::default();
      uni_co_opts.set_size_ratio(2);
      uni_co_opts.set_stop_style(UniversalCompactionStopStyle::Total);
      opts.set_compaction_style(DBCompactionStyle::Universal);
      opts.set_universal_compaction_options(&uni_co_opts);
    }

    // set compaction options
    let mut compact_opts = CompactOptions::default();
    compact_opts.set_exclusive_manual_compaction(true);
    compact_opts.set_target_level(1);
    compact_opts.set_change_level(true);
    compact_opts.set_bottommost_level_compaction(BottommostLevelCompaction::ForceOptimized);

    // put and compact column family cf1
    let cfs = vec!["cf1"];
    let db = DB::open_cf(&opts, &path, cfs).unwrap();
    let cf1 = db.cf_handle("cf1").unwrap();
    db.put_cf(cf1, b"k1", b"v1").unwrap();
    db.put_cf(cf1, b"k2", b"v2").unwrap();
    db.put_cf(cf1, b"k3", b"v3").unwrap();
    db.put_cf(cf1, b"k4", b"v4").unwrap();
    db.put_cf(cf1, b"k5", b"v5").unwrap();
    db.compact_range_cf(cf1, Some(b"k2"), Some(b"k4"));
    db.compact_range_cf_opt(cf1, Some(b"k1"), None::<&str>, &compact_opts);

    // put and compact default column family
    db.put(b"k1", b"v1").unwrap();
    db.put(b"k2", b"v2").unwrap();
    db.put(b"k3", b"v3").unwrap();
    db.put(b"k4", b"v4").unwrap();
    db.put(b"k5", b"v5").unwrap();
    db.compact_range(Some(b"k3"), None::<&str>);
    db.compact_range_opt(None::<&str>, Some(b"k5"), &compact_opts);
  }
}

#[test]
fn fifo_compaction() {
  let path = DBPath::new("_rust_rocksdb_fifo_compaction");
  {
    let mut opts = Options::default();
    opts.create_if_missing(true);
    opts.create_missing_column_families(true);

    // set compaction style
    {
      let mut fifo_co_opts = FifoCompactOptions::default();
      fifo_co_opts.set_max_table_files_size(4 << 10); // 4KB
      opts.set_compaction_style(DBCompactionStyle::Fifo);
      opts.set_fifo_compaction_options(&fifo_co_opts);
    }

    // put and compact column family cf1
    let cfs = vec!["cf1"];
    let db = DB::open_cf(&opts, &path, cfs).unwrap();
    let cf1 = db.cf_handle("cf1").unwrap();
    db.put_cf(cf1, b"k1", b"v1").unwrap();
    db.put_cf(cf1, b"k2", b"v2").unwrap();
    db.put_cf(cf1, b"k3", b"v3").unwrap();
    db.put_cf(cf1, b"k4", b"v4").unwrap();
    db.put_cf(cf1, b"k5", b"v5").unwrap();
    db.compact_range_cf(cf1, Some(b"k2"), Some(b"k4"));

    // check stats
    let ctx = PerfContext::default();

    let block_cache_hit_count = ctx.metric(PerfMetric::BlockCacheHitCount);
    if block_cache_hit_count > 0 {
      let expect = format!("block_cache_hit_count = {}", block_cache_hit_count);
      assert!(ctx.report(true).contains(&expect));
    }
  }
}

#[test]
fn multi_get_cf() {
  let path = DBPath::new("_rust_rocksdb_multi_get_cf");

  {
    let mut opts = Options::default();
    opts.create_if_missing(true);
    opts.create_missing_column_families(true);
    let db = DB::open_cf(&opts, &path, &["cf0", "cf1", "cf2"]).unwrap();

    let cf0 = db.cf_handle("cf0").unwrap();

    let cf1 = db.cf_handle("cf1").unwrap();
    db.put_cf(cf1, b"k1", b"v1").unwrap();

    let cf2 = db.cf_handle("cf2").unwrap();
    db.put_cf(cf2, b"k2", b"v2").unwrap();

    let values = db
      .multi_get_cf(vec![(cf0, b"k0"), (cf1, b"k1"), (cf2, b"k2")])
      .into_iter()
      .map(Result::unwrap)
      .collect::<Vec<_>>();
    assert_eq!(3, values.len());
    assert_eq!(values[0], None);
    assert_eq!(values[1], Some(b"v1".to_vec()));
    assert_eq!(values[2], Some(b"v2".to_vec()));
  }
}

#[test]
fn cuckoo() {
  let path = DBPath::new("_rust_rocksdb_cuckoo");

  {
    let mut opts = Options::default();
    let mut factory_opts = CuckooTableOptions::default();
    factory_opts.set_hash_ratio(0.8);
    factory_opts.set_max_search_depth(20);
    factory_opts.set_cuckoo_block_size(10);
    factory_opts.set_identity_as_first_hash(true);
    factory_opts.set_use_module_hash(false);

    opts.set_cuckoo_table_factory(&factory_opts);
    opts.create_if_missing(true);

    let db = DB::open(&opts, &path).unwrap();
    db.put(b"k1", b"v1").unwrap();
    db.put(b"k2", b"v2").unwrap();
    let r: Result<Option<Vec<u8>>, Error> = db.get(b"k1");

    assert_eq!(r.unwrap().unwrap(), b"v1");
    let r: Result<Option<Vec<u8>>, Error> = db.get(b"k2");

    assert_eq!(r.unwrap().unwrap(), b"v2");
    assert!(db.delete(b"k1").is_ok());
    assert!(db.get(b"k1").unwrap().is_none());
  }
}
