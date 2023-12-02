use log::trace;
use reqwest::{Client, Error};
use std::net::IpAddr;

pub async fn my_ip_verbose() -> Result<serde_json::Value, Error> {
  let echo_json = Client::new()
    .get("https://ipwhois.app/json/")
    .send()
    .await?
    .json()
    .await?;

  trace!("{:#?}", echo_json);
  Ok(echo_json)
}

pub async fn my_ip() -> Result<IpAddr, Error> {
  let res = Client::new()
    .get("https://ipinfo.io/ip")
    .send()
    .await?
    .text()
    .await?;
  trace!("{:#?}", res);
  Ok(res.parse().unwrap())
}

pub async fn get_ip() -> Result<(), Error> {
  let ip = my_ip().await?;
  println!("PUBLIC_IP : {:#?}", ip);
  Ok(())
}
