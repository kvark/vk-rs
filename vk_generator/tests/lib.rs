extern crate vk_generator;
extern crate vk_api;

use std::path::Path;
use std::fs::{File, DirBuilder};
use std::env;
use std::process::Command;
use std::io::Write;
use std::str;

use vk_generator::VkVersion;

#[test]
fn default_global() {
    let out = env::var("OUT_DIR").unwrap();
    DirBuilder::new().recursive(true).create(&out).unwrap();

    let mut file = File::create(&Path::new(&out).join("default_global.rs")).unwrap();
    writeln!(file, "fn main() {{}} mod vk {{").unwrap();
    vk_generator::VkRegistry::new(vk_api::VK_XML).gen_global(&mut file,
                                                             VkVersion(1, 0), 
                                                             &["VK_KHR_surface"],
                                                             Default::default());
    writeln!(file, "}}").unwrap();

    assert_eq!("", str::from_utf8(&Command::new("rustc").current_dir(&out).arg("default_global.rs").output().unwrap().stderr).unwrap());
}

#[test]
fn default_struct() {
    let out = env::var("OUT_DIR").unwrap();
    DirBuilder::new().recursive(true).create(&out).unwrap();

    let mut file = File::create(&Path::new(&out).join("default_struct.rs")).unwrap();
    writeln!(file, "fn main() {{}} mod vk {{").unwrap();
    vk_generator::VkRegistry::new(vk_api::VK_XML).gen_struct(&mut file,
                                                             VkVersion(1, 0), 
                                                             &["VK_KHR_surface"],
                                                             Default::default());
    writeln!(file, "}}").unwrap();

    assert_eq!("", str::from_utf8(&Command::new("rustc").current_dir(&out).arg("default_struct.rs").output().unwrap().stderr).unwrap());
}