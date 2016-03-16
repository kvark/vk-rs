extern crate vk_api;
extern crate xml;

use std::fs::File;
use std::io::BufWriter;
use std::io::Write;

use xml::EventReader;
use xml::reader::XmlEvent;
use xml::name::OwnedName;
use xml::attribute::OwnedAttribute;

pub fn load_xml() {
    let output = File::create("output.txt").unwrap();
    let mut output = BufWriter::with_capacity(20000*1024, output);
    let vk_xml = EventReader::new(vk_api::VK_XML);

    let mut vk_elements = Vec::with_capacity(10);
    let mut delta = true;
    for event in vk_xml.into_iter() {
        let event = event.unwrap();
        match event {
            XmlEvent::StartElement{ name, attributes, .. } => {
                vk_elements.push(XmlElement::new_tag(name, attributes));
                delta = true;
            }

            XmlEvent::EndElement{ name } => {
                pop_element_stack(&mut vk_elements, name);
            }

            XmlEvent::Characters(chars) => {
                vk_elements.push(XmlElement::Characters(chars));
                delta = true;
            }

            _ => ()
        }

        if delta == true {
            write!(&mut output, "{:#?}\n", vk_elements).unwrap();
        }
        delta = false;
    }
}

fn pop_element_stack(vk_elements: &mut Vec<XmlElement>, end_name: OwnedName) {
    match vk_elements.pop() {
        Some(el) => 
            if let XmlElement::Characters(_) = el {
                pop_element_stack(vk_elements, end_name)
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
        attributes: Vec<VkAttribute>
    },
    Characters( String )
}

impl XmlElement {
    fn new_tag(name: OwnedName, attributes: Vec<OwnedAttribute>) -> XmlElement {
        XmlElement::Tag {
            name: name.local_name,
            attributes: attributes.into_iter().map(
                |a| VkAttribute {
                    name: a.name.local_name,
                    value: a.value
                }).collect()
        }
    }
}

#[derive(Debug)]
struct VkAttribute{
    name: String,
    value: String
}