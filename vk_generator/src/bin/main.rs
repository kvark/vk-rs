extern crate vk_generator;
extern crate vk_api;

fn main() {
    vk_generator::VkRegistry::new(vk_api::VK_XML);
}