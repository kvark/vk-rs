//! Module that contains a crawler function that crawls through the vulkan xml and returns a 
//! Registry struct
use xml::reader::{Events, XmlEvent};
use xml::name::OwnedName;
use xml::attribute::OwnedAttribute;
use std::io::Read;
use std::ffi::CStr;
use {VkRegistry, VkType};
use VkVariant;

pub fn crawl<R: Read>(xml_events: Events<R>, mut registry: VkRegistry) -> VkRegistry {
    use self::XmlElement::*;

    let mut type_buffer = VkType::None;
    let mut cur_block = VkBlock::None;
    // A variable that contains what the index of the element in vk_elements that vk_elements was
    // popped to. Used to prevent the element iterator from going over elements that have already
    // been checked.
    let mut popped_to = 0;

    // A stack of the relevant elements in the vulkan xml
    let mut vk_elements = Vec::with_capacity(10);

    for event in xml_events {
        
        let event = event.unwrap();
        match event {
            XmlEvent::StartElement{ name, attributes, .. } => {
                vk_elements.push(XmlElement::new_tag(name, attributes));
            }

            XmlEvent::EndElement{ name } => {
                for el in &vk_elements[popped_to..] {

                    match *el {
                        Tag{name: ref tag_name, attributes: ref tag_attrs} => {
                            match &**tag_name {
                                "types" => cur_block = VkBlock::Types,

                                "type"  =>
                                    match cur_block {
                                        VkBlock::Types => {
                                            if let Some(category) = find_attribute(tag_attrs, "category") {
                                                registry.push_type(type_buffer).ok();
                                                match category {
                                                    "basetype"      => type_buffer = VkType::None,
                                                    "bitmask"       => type_buffer = VkType::None,
                                                    "define"        => type_buffer = VkType::None,
                                                    "enum"          => type_buffer = VkType::None,
                                                    "funcpointer"   => type_buffer = VkType::None,
                                                    "group"         => type_buffer = VkType::None,
                                                    "handle"        => type_buffer = VkType::None,
                                                    "include"       => type_buffer = VkType::None,
                                                    "struct"        => 
                                                        type_buffer = VkType::Struct{name: registry.append_str(find_attribute(tag_attrs, "name").unwrap()), fields: Vec::with_capacity(8)},
                                                    "union"         => type_buffer = VkType::None,
                                                    _               => panic!("Unexpected category")
                                                }}
                                        }
                                        VkBlock::Enums => panic!("Unexpected \"type\" tag found in \"enums\" block"),
                                        VkBlock::None => panic!("\"type\" tag found outside of \"types\" block")
                                    },

                                _ => ()
                            }
                        }

                        Characters(_) => ()
                    }
                }

                pop_element_stack(&mut vk_elements);
                popped_to = 
                    match vk_elements.len() {
                        0 => 0,
                        not0 => not0 - 1
                    };
            }

            XmlEvent::Characters(chars) => {
                vk_elements.push(XmlElement::Characters(chars));
            }

            _ => ()
        }
    }

    println!("{:#?}", (&registry.types).into_iter().map(|t| if let &VkType::Struct{name, ..} = t {Some(unsafe{ CStr::from_ptr(name) })} else {None}).collect::<Vec<_>>());

    registry
}

fn find_attribute<'v>(source: &'v Vec<OwnedAttribute>, query: &str) -> Option<&'v str> {
    source.into_iter().skip_while(|attr| &attr.name.local_name != query).next().map(|res| &*res.value)
}

/// Takes a mutable reference to a XmlElement stack, popping the stack of Character elements until
/// it pops a Tag
fn pop_element_stack(vk_elements: &mut Vec<XmlElement>) {
    match vk_elements.pop() {
            Some(el) => 
                if let XmlElement::Characters(_) = el {
                    pop_element_stack(vk_elements)
                },

            None => panic!("Invalid xml; Unexpected closing tag")
    }
}

#[derive(Debug)]
enum XmlElement {
    Tag {
        /// The name of the tag e.g. "hello" in <hello>
        name: String,
        /// The attributes contained within the specification xml.
        attributes: Vec<OwnedAttribute>
    },
    Characters( String )
}

impl XmlElement {
    fn new_tag(name: OwnedName, attributes: Vec<OwnedAttribute>) -> XmlElement {
        XmlElement::Tag {
            name: name.local_name,
            attributes: attributes
        }
    }
}

enum VkBlock {
    Types,
    Enums,
    None
}