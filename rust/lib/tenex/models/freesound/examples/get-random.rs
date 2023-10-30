use freesound::{write_sound, FreeSoundRequest, FreeSoundResponse, FreeSoundClient, Result, ClientConfig};
use std::{env, path::Path};

#[tokio::main]
async fn main() -> Result<()> {
  let args: Vec<String> = std::env::args().collect();
  let mut args = args.iter().skip(1);
  let cmd = args.next();
  let cfg_var = env::var("FREESOUND_CONFIG");
  let config_path = match cfg_var {
    Ok(ref val) => Path::new(val),
    Err(_) => Path::new("freesound.json")
  };
  let config = ClientConfig::load(&config_path)?;
  let mut client = FreeSoundClient::new_with_config(&config);
  if let Some(cmd) = cmd {
    if cmd.eq("auth") {
      client.auth(true).await.unwrap();
      client.save(&config_path)?;
    } else if cmd.eq("search") {
      let req = FreeSoundRequest::SearchText {
	query: &args.next().unwrap(),
	filter: None,
	sort: "",
	group_by_pack: false,
	weights: "",
	page: 1,
	page_size: 150,
	fields: &["id", "name"],
	descriptors: &[],
	normalized: false,
      };
      let res = client.request(req).await.unwrap();
      let response = FreeSoundResponse::parse(res).await;
      println!("{}", response);
    } else if cmd.eq("dl") || cmd.eq("download") {
      let query = args.next().unwrap();
      let out = if let Some(p) = args.next() {
	p
      } else {
	query
      };
      let req = FreeSoundRequest::SoundDownload {
	id: query.parse().unwrap(),
      };
      let res = client.request(req).await.unwrap();
      write_sound(res, &out, true).await.unwrap();
      println!("sound_id {} downloaded to {}", query, out.as_str());
    }
  } else {
    println!("tenex-fs [auth|query|download/dl] ARG");
  }
  Ok(())
}
