extern crate vk_generator;
extern crate vk_api;

use std::fs::File;

use vk_generator::registry::VkVersion;

fn main() {
    vk_generator::VkRegistry::new(vk_api::VK_XML).gen_global(&mut File::create("vk.rs").unwrap(),
                                                             VkVersion(1, 0), 
                                                             &["VK_KHR_surface"],
                                                             Default::default());
}