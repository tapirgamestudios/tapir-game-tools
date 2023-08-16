#![deny(clippy::all)]

use std::hash::Hash;

mod cable_state;
mod cables;
mod port;

pub use cable_state::PortId;
pub use cables::cables;
pub use port::{port, PortDirection};

pub(crate) use cable_state::CableState;

pub trait BlockId: Hash + Clone + Copy + PartialEq + Eq + Sync + Send {}

impl<T> BlockId for T where T: Hash + Clone + Copy + PartialEq + Eq + Sync + Send {}
