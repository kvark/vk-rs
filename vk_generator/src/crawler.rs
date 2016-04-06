//! Module that contains a crawler function that crawls through the vulkan xml and returns a 
//! Registry struct
use xml::reader::{Events, XmlEvent};
use xml::name::OwnedName;
use xml::attribute::OwnedAttribute;
use std::io::Read;
use std::ffi::CStr;
use {VkRegistry, VkType, VkMember};
use VkVariant;

pub fn crawl<R: Read>(xml_events: Events<R>, mut registry: VkRegistry) -> VkRegistry {
    use self::XmlElement::*;

    let mut type_buffer = VkType::Unhandled;
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
                        Tag{name: ref tag_name, attributes: ref tag_attrs} => 
                            match &tag_name[..] {
                                "types"      => cur_block = VkBlock::Types,
                                "enums"      => cur_block = VkBlock::Enums,
                                "commands"   => cur_block = VkBlock::Commands,
                                "extensions" => cur_block = VkBlock::Extensions,

                                "type"  =>
                                    if VkBlock::Types == cur_block {
                                        if let Some(category) = find_attribute(tag_attrs, "category") {
                                            registry.push_type(type_buffer).ok();
                                            match category {
                                                "basetype"      => type_buffer = VkType::Unhandled,
                                                "bitmask"       => type_buffer = VkType::Unhandled,
                                                "define"        => type_buffer = VkType::Unhandled,
                                                "enum"          => type_buffer = VkType::Unhandled,
                                                "funcpointer"   => type_buffer = VkType::Unhandled,
                                                "group"         => type_buffer = VkType::Unhandled,
                                                "handle"        => type_buffer = VkType::Unhandled,
                                                "include"       => type_buffer = VkType::Unhandled,
                                                "struct"        => 
                                                    type_buffer = VkType::Struct{name: registry.append_str(find_attribute(tag_attrs, "name").unwrap()), fields: Vec::with_capacity(8)},
                                                "union"         => 
                                                    type_buffer = VkType::Union{name: registry.append_str(find_attribute(tag_attrs, "name").unwrap()), variants: Vec::with_capacity(8)},
                                                _               => panic!("Unexpected category")
                                            }
                                        }
                                    } else {/*panic!("\"type\" tag found outside of \"types\" block")*/}, //TODO: implement fully so panic can be used

                                "member" =>
                                    if VkBlock::Types == cur_block {
                                        match type_buffer {
                                            VkType::Struct{ref mut fields, ..} => fields.push(VkMember::empty()),
                                            VkType::Union{ref mut variants, ..} => variants.push(VkMember::empty()),
                                            _ =>
                                                panic!("Unexpected \"member\" tag found")
                                        }
                                    } else {panic!("\"member\" tag found outside of \"types\" block")},

                                _ => ()
                            },

                        Characters{ref chars, tag} =>
                            if VkBlock::Types == cur_block {
                                match type_buffer {
                                    VkType::Struct{fields: ref mut members, ..} |
                                    VkType::Union{variants: ref mut members, ..} =>
                                        match tag {
                                            "member" =>
                                                match chars.trim() {
                                                    "const" => members.last_mut().unwrap().change_type_const(),
                                                    "*"     => members.last_mut().unwrap().change_type_ptr(),
                                                    _       => ()
                                                },
                                            "type" =>
                                                members.last_mut().unwrap().set_type(registry.append_str(&chars[..])),
                                            "name" =>
                                                members.last_mut().unwrap().set_name(registry.append_str(&chars[..])),
                                            _ => ()
                                        },
                                    _ => ()
                                }
                            }
                    }
                }

                pop_element_stack(&mut vk_elements);
                popped_to = vk_elements.len();
            }

            XmlEvent::Characters(chars) => {
                let tag_ptr =
                    match *vk_elements.last().unwrap() {
                        XmlElement::Tag{name: ref tag, ..} => &tag[..] as *const str,
                        // In circumstances where the xml reads something like <foo>foostart<bar>bar</bar>fooend</foo>, the
                        // "fooend" string will appear in vk_elements after the "foostart" event, instead of after <foo>.
                        // Because of that, we must pull the tag from the other Characters event.
                        XmlElement::Characters{tag, ..} => tag as *const str
                    };

                unsafe{ vk_elements.push(XmlElement::new_chars(chars, &*tag_ptr)) }
            }

            _ => ()
        }
    }

    for t in &registry.types {
        unsafe {
            match *t {
                VkType::Struct{name, ref fields} => {
                    println!("Struct {:?}", CStr::from_ptr(name));

                    for f in fields {
                        println!("{:#?}", f);
                    }
                }
                VkType::Union{name, ref variants} => {
                    println!("Union {:?}", CStr::from_ptr(name));

                    for v in variants {
                        println!("{:#?}", v);
                    }
                }
                _ => ()
            }
        }
    }

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
            if let XmlElement::Characters{..} = el {
                pop_element_stack(vk_elements)
            },

        None => panic!("Invalid xml; Unexpected closing tag")
    }
}

#[derive(Debug)]
enum XmlElement<'a> {
    Tag {
        /// The name of the tag e.g. "hello" in <hello>
        name: String,
        /// The attributes contained within the specification xml.
        attributes: Vec<OwnedAttribute>
    },
    Characters{
        chars: String,
        tag: &'a str
    }
}

impl<'a> XmlElement<'a> {
    fn new_tag(name: OwnedName, attributes: Vec<OwnedAttribute>) -> XmlElement<'a> {
        XmlElement::Tag {
            name: name.local_name,
            attributes: attributes
        }
    }

    fn new_chars(chars: String, tag: &'a str) -> XmlElement<'a> {
        XmlElement::Characters {
            chars: chars,
            tag: tag
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum VkBlock {
    Types,
    Enums,
    Commands,
    Extensions,
    None
}