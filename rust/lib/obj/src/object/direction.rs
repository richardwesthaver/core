//! # Direction types
use crate::{Deserialize, Error, Serialize};
use std::str::FromStr;
#[derive(Debug, Default, Serialize, Deserialize)]
pub enum CardinalDirection {
  #[default]
  North,
  South,
  East,
  West,
}

impl FromStr for CardinalDirection {
  type Err = Error;

  fn from_str(s: &str) -> Result<CardinalDirection, Self::Err> {
    match s {
      "north" => Ok(CardinalDirection::North),
      "south" => Ok(CardinalDirection::South),
      "east" => Ok(CardinalDirection::East),
      "west" => Ok(CardinalDirection::West),
      _ => Err(Error::Message("not a CardinalDirection".to_string())),
    }
  }
}

impl From<CardinalDirection> for String {
  fn from(d: CardinalDirection) -> Self {
    match d {
      CardinalDirection::North => "north".to_string(),
      CardinalDirection::South => "south".to_string(),
      CardinalDirection::East => "east".to_string(),
      CardinalDirection::West => "west".to_string(),
    }
  }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub enum RelativeDirection {
  Up,
  #[default]
  Down,
  Left,
  Right,
}

impl FromStr for RelativeDirection {
  type Err = Error;

  fn from_str(s: &str) -> Result<RelativeDirection, Self::Err> {
    match s {
      "up" | "u" => Ok(RelativeDirection::Up),
      "down" | "d" => Ok(RelativeDirection::Down),
      "left" | "l" => Ok(RelativeDirection::Left),
      "right" | "r" => Ok(RelativeDirection::Right),
      _ => Err(Error::Message("not a RelativeDirection".to_string())),
    }
  }
}

impl From<RelativeDirection> for String {
  fn from(d: RelativeDirection) -> Self {
    match d {
      RelativeDirection::Up => "up".to_string(),
      RelativeDirection::Down => "down".to_string(),
      RelativeDirection::Left => "left".to_string(),
      RelativeDirection::Right => "right".to_string(),
    }
  }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash, Copy, Serialize, Deserialize)]
pub enum EdgeDirection {
  Outbound,
  Inbound,
}

impl FromStr for EdgeDirection {
  type Err = Error;

  fn from_str(s: &str) -> Result<EdgeDirection, Self::Err> {
    match s {
      "outbound" => Ok(EdgeDirection::Outbound),
      "inbound" => Ok(EdgeDirection::Inbound),
      _ => Err(Error::Message("not an EdgeDirection".to_string())),
    }
  }
}

impl From<EdgeDirection> for String {
  fn from(d: EdgeDirection) -> Self {
    match d {
      EdgeDirection::Outbound => "outbound".to_string(),
      EdgeDirection::Inbound => "inbound".to_string(),
    }
  }
}
