extern crate xml;
extern crate boolinator;

use std::mem;

pub mod registry;
pub mod generator;

pub use registry::{VkRegistry, VkVersion};
pub use generator::GenConfig;

#[inline]
fn to_option<'u>(s: *const str) -> Option<&'u str> {
    unsafe{ mem::transmute(s) }
}