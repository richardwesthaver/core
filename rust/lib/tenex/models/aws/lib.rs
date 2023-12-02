//! aws/lib.rs --- AWS client model
pub use aws_config::{meta::region::RegionProviderChain, Region};
pub fn new_region(region: Option<String>) -> RegionProviderChain {
  RegionProviderChain::first_try(region.map(Region::new))
    .or_default_provider()
    .or_else(Region::new("us-east-2"))
}

//// Route53
pub async fn show_host_info(
  client: &aws_sdk_route53::Client,
) -> Result<(), aws_sdk_route53::Error> {
  let hosted_zone_count = client.get_hosted_zone_count().send().await?;

  println!(
    "Number of hosted zones in region : {}",
    hosted_zone_count.hosted_zone_count(),
  );

  let hosted_zones = client.list_hosted_zones().send().await?;

  println!("Zones:");

  for hz in hosted_zones.hosted_zones() {
    let zone_name = hz.name();
    let zone_id = hz.id();

    println!("  ID :   {}", zone_id);
    println!("  Name : {}", zone_name);
    println!();
  }

  Ok(())
}

//// EC2
// pub async fn start_instance(client: &aws_sdk_ec2::Client, id: &str) ->
// Result<(), aws_sdk_ec2::Error> {     client.start_instances().
// instance_ids(id).send().await?;

//     println!("Started instance.");

//     Ok(())
// }

// pub async fn delete_snapshot(client: &aws_sdk_ec2::Client, id: &str) ->
// Result<(), aws_sdk_ec2::Error> {     client.delete_snapshot().
// snapshot_id(id).send().await?;     Ok(())
// }
