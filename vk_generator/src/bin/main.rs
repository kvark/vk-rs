extern crate vk_generator;
extern crate vk_api;

use vk_generator::registry::VkVersion;

fn main() {
    vk_generator::VkRegistry::new(vk_api::VK_XML).gen_global(VkVersion(1, 0), 
                                                             &["VK_KHR_surface"],
                                                             Default::default());
}