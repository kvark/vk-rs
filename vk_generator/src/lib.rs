extern crate vk_api;
extern crate xml;

mod crawler;

use xml::EventReader;
use xml::attribute::OwnedAttribute;

use std::os::raw::c_char;

pub fn load_xml() {
    let vk_xml = EventReader::new(vk_api::VK_XML);
    // The vulkan registry
    let registry = crawler::crawl(vk_xml.into_iter(), VkRegistry::new(vk_api::VK_XML.len()));
}

pub struct VkRegistry {
    string_buffer: String,
    types: Vec<VkType>
}

impl<'a> VkRegistry {
    fn new(capacity: usize) -> VkRegistry {
        VkRegistry {
            string_buffer: String::with_capacity(capacity),
            types: Vec::with_capacity(256)
        }
    }

    fn push_type(&mut self, vk_type: VkType) -> Result<(), ()> {
        match vk_type {
            VkType::None => Err(()),
            vk_type      => {self.types.push(vk_type); Ok(())}
        }
    }

    /// Append a given attribute to the internal string buffer and return a pointer to a CStr that represents the attribute
    fn append_str<'s>(&mut self, string: &str) -> *const c_char {
        // We want to have all of the string in one block of memory in order to save heap allocation time. 
        self.string_buffer.push_str(string);
        self.string_buffer.push('\0');

        (self.string_buffer.as_ptr() as usize + self.string_buffer.len() - string.len() - 1) as *const c_char
    }
}

#[derive(Debug)]
struct VkField {
    field_type: *const c_char,
    field_name: *const c_char
}

/// A variant of a vulkan enum
#[derive(Debug)]
struct VkVariant {
    name: *const c_char,
    value: isize
}

#[derive(Debug)]
enum VkType {
    Struct {
        name: *const c_char,
        fields: Vec<VkField>
    },

    Enum {
        name: *const c_char,
        variants: Vec<VkVariant>
    },

    BaseType {
        /// The type that is being aliased
        ty: *const c_char,
        /// The name of the new type
        name: *const c_char
    },

    /// A dummy variant used to initialize the type creation system
    None
}