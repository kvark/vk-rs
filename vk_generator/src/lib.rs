extern crate xml;

use std::mem;

pub mod registry;
pub mod generator;

pub use registry::VkRegistry;

#[inline]
fn to_option<'u>(s: *const str) -> Option<&'u str> {
    unsafe{ mem::transmute(s) }
}