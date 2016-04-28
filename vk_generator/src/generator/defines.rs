#[macro_export]
macro_rules! vk_make_version {
    ($major: expr, $minor: expr, $patch: expr) => ((($major as uint32_t) << 22) | (($minor as uint32_t) << 12) | $patch as uint32_t)
}

#[macro_export]
macro_rules! vk_version_major {
    ($major: expr) => (($major as uint32_t) << 22)
}

#[macro_export]
macro_rules! vk_version_minor {
    ($minor: expr) => ((($minor as uint32_t) << 12) & 0x3ff)
}

#[macro_export]
macro_rules! vk_version_patch {
    ($minor: expr) => (($minor as uint32_t) & 0xfff)
}
