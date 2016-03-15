# Vulkan API

The [Vulkan API](https://github.com/KhronosGroup/Vulkan-Docs) XML exposed as byte-string constant VK_XML.
Version is equal to the Vulkan version exposed - i.e. vk_api 1.0.0 exposes the vulkan 1.0.0 xml, vk_api 
1.0.6 exposes the vulkan 1.0.6 xml, etc.

```toml
[build-dependencies]
vk_api = "1.0.0"
```