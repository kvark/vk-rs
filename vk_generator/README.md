# vk_generator [![Version](https://img.shields.io/crates/v/vk_generator.svg)](https://crates.io/crates/vk_generator)
Rust binding generator for the [Vulkan API](https://www.khronos.org/vulkan/).

## [Documentation](https://docs.rs/vk_generator/)

## Custom Generators
`vk_generator` supports the usage of custom generators, which can be implemented as traits on the
`VkRegistry` type. To do so, one must enable the `unstable_generator_api` cargo feature. It is
important to note that, as the feature name suggests, the internal API is entirely unstable and may
be subject to change at any point.