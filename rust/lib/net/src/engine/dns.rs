pub mod client;
pub mod resolver;

use std::{collections::HashMap, net::IpAddr};

pub type IpMap = HashMap<IpAddr, String>;
