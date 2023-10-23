use super::SubRepo;
use crate::Result;
use hg_parser::{file_content, FileType, ManifestEntryDetails, MercurialRepository, Revision};

use logger::log::{error, info, trace};
use serde::{Deserialize, Serialize};
use std::{
  collections::HashMap,
  fs::File,
  io::{LineWriter, Read, Write},
  net::SocketAddr,
  path::{Path, PathBuf},
  time::Instant,
};

/// Mercurial configuration type -- corresponds to .hgrc file
///
/// TODO set 'default' and 'default_push' paths
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct MercurialConfig {
  pub ui: HashMap<String, String>,
  pub extensions: Option<HashMap<String, Option<String>>>,
  pub paths: Option<HashMap<String, String>>,
  pub web: HgwebConfig,
}

impl MercurialConfig {
  pub fn handle<P: AsRef<Path>>(path: P) -> MercurialRepository {
    MercurialRepository::open(path).unwrap()
  }
}

/// Mercurial '.hgsub' file handle, which is just a list of PATH=SOURCE pairs.
pub struct HgSubFile {
  pub path: PathBuf,          // path to the .hgsub file
  pub subrepos: Vec<SubRepo>, // Vec containing `SubRepo` handles
}

impl HgSubFile {
  /// Create a new '.hgsub' file handle
  pub fn new() -> Self {
    HgSubFile {
      path: PathBuf::from(".hgsub"),
      subrepos: vec![],
    }
  }
  /// ensure that the path is in a hg repo.
  fn parent_is_hg(&self) -> bool {
    if self.path.parent().unwrap().join(".hg").exists() {
      true
    } else {
      false
    }
  }
  /// insert a subrepo into this HgSubFile. does not clone the source
  /// or ensure that path exists. Takes an optional argument of 'hg'
  /// or 'git' to indicate the subrepo-type. Value can be ommitted to
  pub fn insert(&mut self, path: &str, source: &str, vcs: Option<&str>) -> Result<()> {
    let mut prefix = "";
    // set prefix based on vcs (repo type)
    if let Some(i) = vcs {
      match i {
        "hg" => prefix = "hg",
        "git" => prefix = "git",
        _ => {
          error!("failed to recognize repo type")
        }
      }
    }

    let source = format!("{}:{}", prefix, source);

    let subrepo = SubRepo {
      vcs: vcs.unwrap().to_string(),
      origin: source.to_string(),
      path: path.to_string(),
    };

    self.subrepos.push(subrepo);
    Ok(())
  }

  /// Save subs to `.hgsub` file specified in the `path` field of
  /// this struct. This will overwrite any existing file at that path.
  pub fn save(self) -> Result<()> {
    match self.parent_is_hg() {
      true => {
        let mut file = File::open(self.path).unwrap();
        for i in self.subrepos.iter() {
          write!(file, "{} = {}", i.path, i.origin)?;
        }
      }
      false => {
        error!("Parent is not a Mercurial repo!")
      }
    }
    Ok(())
  }

  /// Sort the full contents of a .hgsub file alphabetically.
  pub fn sort(self) -> Result<Self> {
    let mut fd = File::open(&self.path)?;
    let len = fd.metadata().unwrap().len() as usize;
    let mut bufr = String::with_capacity(len);
    fd.read_to_string(&mut bufr).unwrap(); // This reads the entire file into memory.
    drop(fd); // drop the old readonly file descriptor

    let mut subs = Vec::new();

    trace!("starting sort of {:?} lines", &self.path.canonicalize()?);
    for line in bufr.lines() {
      subs.push(line.as_bytes());
    }
    subs.sort_unstable();
    trace!("lines have been sorted");

    let file = File::create(&self.path)?;
    let mut file = LineWriter::new(file);
    for sub in subs.iter() {
      file.write_all(sub)?;
      file.write_all(b"\n")?;
    }

    file.flush()?;
    info!("sorted {:?}", &self.path.canonicalize()?);

    Ok(self)
  }
}

impl Default for HgSubFile {
  fn default() -> Self {
    HgSubFile {
      path: PathBuf::from(".hgsub"),
      subrepos: vec![],
    }
  }
}

/// Hgweb configuration type
///
/// Based on the configuration file for 'hgweb' scripts.
///
/// We don't store the file path in a field because all HgwebConfig
/// values are relative to env::current_dir()
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct HgwebConfig {
  pub name: String,
  pub contact: String,
  pub description: String,
  pub extensions: Vec<String>,
  pub socket: SocketAddr,
  pub paths: HashMap<PathBuf, PathBuf>,
}

impl Default for HgwebConfig {
  fn default() -> Self {
    HgwebConfig {
      name: "".to_string(),
      contact: "".to_string(),
      description: "".to_string(),
      extensions: vec![],
      socket: "0.0.0.0:0"
        .parse()
        .expect("could not parse hgweb socketaddr"),
      paths: HashMap::new(),
    }
  }
}

/// from hg_parser crate docs - given a mercurial repo as a path,
/// exports to git fast-import format (to stdout)
pub fn export_hg_git<P: AsRef<Path>>(path: P) -> Result<()> {
  let start = Instant::now();
  let repo = MercurialRepository::open(path).expect("could not open repo path");

  let stdout = std::io::stdout();
  let mut writer = stdout.lock();

  for changeset in &repo {
    let revision = changeset.revision;
    eprintln!("rev: {:?}", revision);

    let header = &changeset.header;
    let mut branch = None;
    let mut closed = false;
    for (key, value) in &header.extra {
      if key == b"branch" {
        branch = Some(value.as_slice());
      }

      if key == b"close" && value == b"1" {
        closed = true;
      }
    }

    let mut branch: Vec<_> = branch.unwrap_or_else(|| b"master").into();
    for b in branch.iter_mut() {
      if *b == b' ' {
        *b = b'-';
      }
    }

    let user = String::from_utf8_lossy(&header.user);
    let desc = String::from_utf8_lossy(&header.comment);

    let time = header.time.timestamp_secs();
    let timezone = header.time.tz_offset_secs();
    let tz = format!("{:+03}{:02}", -timezone / 3600, ((-timezone % 3600) / 60));

    write!(writer, "reset refs/heads/")?;
    writer.write_all(&mut branch)?;
    write!(writer, "\ncommit refs/heads/")?;
    writer.write_all(&mut branch)?;
    writeln!(writer, "\nmark :{}", mark(revision))?;

    writeln!(writer, "author {} {} {}", user, time, tz)?;
    writeln!(writer, "committer {} {} {}", user, time, tz)?;
    writeln!(writer, "data {}", desc.len() + 1)?;
    writeln!(writer, "{}\n", desc)?;

    match (header.p1, header.p2) {
      (Some(p1), Some(p2)) => {
        writeln!(writer, "from :{}", mark(p1))?;
        writeln!(writer, "merge :{}", mark(p2))?;
      }
      (Some(p), None) | (None, Some(p)) => {
        writeln!(writer, "from :{}", mark(p))?;
      }
      _ => (),
    }

    for mut file in changeset.files {
      match (file.data, file.manifest_entry) {
        (None, None) => {
          write!(writer, "D ")?;
          writer.write_all(&mut file.path)?;
          writeln!(writer)?;
        }
        (Some(data), Some(manifest_entry)) => {
          write!(
            writer,
            "M {} inline ",
            match manifest_entry.details {
              ManifestEntryDetails::File(FileType::Symlink) => "120000",
              ManifestEntryDetails::File(FileType::Executable) => "100755",
              ManifestEntryDetails::Tree | ManifestEntryDetails::File(FileType::Regular) =>
                "100644",
            }
          )?;
          writer.write_all(&mut file.path)?;
          let data = file_content(&data);
          writeln!(writer, "\ndata {}", data.len())?;
          writer.write_all(&data[..])?;
        }
        _ => panic!("Wrong file data!"),
      }
    }

    if closed {
      write!(writer, "reset refs/tags/archive/")?;
      writer.write_all(&mut branch)?;
      writeln!(writer, "\nfrom :{}\n", mark(revision))?;

      write!(writer, "reset refs/heads/")?;
      writer.write_all(&mut branch)?;
      writeln!(writer, "\nfrom 0000000000000000000000000000000000000000\n")?;
    }
  }

  for (rev, tag) in repo.tags().unwrap() {
    eprintln!("export tag {}", tag.name);
    writeln!(writer, "reset refs/tags/{}", tag.name).unwrap();
    writeln!(writer, "from :{}", mark(rev)).unwrap();
    writeln!(writer).unwrap();
  }

  eprintln!("Done. Elapsed: {:?}", start.elapsed());
  Ok(())
}

fn mark<R: Into<Revision>>(rev: R) -> usize {
  (rev.into() + 1).0 as usize
}
