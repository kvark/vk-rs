//! Module that contains a crawler function that crawls through the vulkan xml and returns a 
//! Registry struct
use xml::reader::{Events, XmlEvent};
use xml::name::OwnedName;
use xml::attribute::OwnedAttribute;
use std::io::Read;
use std::slice::Iter;
use std::num::ParseIntError;
use super::{VkRegistry, VkType, VkMember, VkVariant, VkCommand, VkParam, VkFeature, VkVersion, VkReqRem, VkExtn};

pub fn crawl<R: Read>(xml_events: Events<R>, registry: &mut VkRegistry) {
    use self::XmlElement::*;

    let mut type_buffer = VkType::Unhandled;
    let mut command_buffer: Option<VkCommand> = None;
    let mut feature_buffer: Option<VkFeature> = None;
    let mut interface_reqrem = VkReqRem::None;
    let mut extn_buffer: Option<VkExtn> = None;
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

            XmlEvent::EndElement{..} => {
                for el in &vk_elements[popped_to..] {
                    use super::tdvalid::*;
                    match *el {
                        Tag{name: ref tag_name, attributes: ref tag_attrs} => 
                            match &tag_name[..] {
                                "enums"      => 
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        cur_block = VkBlock::Enums;
                                        registry.push_type(type_buffer).ok();
                                        type_buffer = VkType::new_enum(registry.append_str(name));
                                    } else {panic!("Could not find enum name")},
                                "enum"
                                    if VkBlock::Enums == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        if let VkType::Enum{name: enum_name, ref mut variants} = type_buffer {
                                            let name = registry.append_str(name);

                                            variants.push(
                                                if "API Constants" == unsafe{ &*enum_name } {
                                                    if let Some(value) = find_attribute(tag_attrs, "value") {
                                                        VkVariant::new_const(name, registry.append_str(value))
                                                    } else {panic!("Could not find value in API Constant")}

                                                } else if let Some(value) = find_attribute(tag_attrs, "value") {
                                                    VkVariant::new_value(name, to_isize(value).unwrap())
                                                } else if let Some(bitpos) = find_attribute(tag_attrs, "bitpos") {
                                                    VkVariant::new_bitpos(name, to_isize(bitpos).unwrap())
                                                } else {panic!("Could not find value or bitpos in enum")}
                                            );
                                        }
                                    } else {panic!("Could not find enum variant name")},

                                "types"      => cur_block = VkBlock::Types,
                                "type"
                                    if VkBlock::Types == cur_block =>
                                    if let Some(category) = find_attribute(tag_attrs, "category") {
                                        registry.push_type(type_buffer).ok();
                                        match category {
                                            "basetype"       |
                                            "bitmask"       => type_buffer = VkType::empty_typedef(),
                                            "define"        => 
                                                if let Some(name) = find_attribute(tag_attrs, "name") {
                                                    type_buffer = VkType::new_define(registry.append_str(name));
                                                } else {type_buffer = VkType::empty_define()},
                                            "enum"          => type_buffer = VkType::Unhandled,
                                            "funcpointer"   => type_buffer = VkType::empty_funcpointer(),
                                            "group"         => type_buffer = VkType::Unhandled,
                                            "handle"        => type_buffer = VkType::empty_handle(),
                                            "include"       => type_buffer = VkType::Unhandled,
                                            "struct"        => 
                                                type_buffer = VkType::new_struct(registry.append_str(find_attribute(tag_attrs, "name").unwrap())),
                                            "union"         => 
                                                type_buffer = VkType::new_union(registry.append_str(find_attribute(tag_attrs, "name").unwrap())),
                                            _               => panic!("Unexpected category")
                                        }
                                    },
                                "member"
                                    if VkBlock::Types == cur_block =>
                                    match type_buffer {
                                        VkType::Struct{fields: ref mut members, ..}   |
                                        VkType::Union{variants: ref mut members, ..} => 
                                            if let Some("true") = find_attribute(tag_attrs, "optional") {
                                                members.push(VkMember::optional())
                                            } else {members.push(VkMember::empty())},
                                        _                                            => panic!("Unexpected \"member\" tag found")
                                    },
                                "member"     => panic!("\"member\" tag found outside of \"types\" block"),

                                "commands"   => cur_block = VkBlock::Commands,
                                "command"
                                    if VkBlock::Commands == cur_block => {
                                        registry.push_command(command_buffer).ok();
                                        command_buffer = Some(VkCommand::empty());
                                    }
                                "param"
                                    if VkBlock::Commands == cur_block => command_buffer.as_mut().unwrap().params.push(VkParam::empty()),

                                "feature"    => 
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        if let Some(version) = find_attribute(tag_attrs, "number") {
                                            cur_block = VkBlock::Feature;
                                            registry.push_feature(feature_buffer).ok();
                                            feature_buffer = Some(VkFeature::new(registry.append_str(name), VkVersion::from_str(version)));
                                        } else {panic!("Could not find feature number")}
                                    } else {panic!("Could not find feature name")},
                                "require"
                                    if VkBlock::Feature == cur_block => 
                                    interface_reqrem = VkReqRem::Require(find_attribute(tag_attrs, "profile").map(|s| registry.append_str(s))),
                                "remove"
                                    if VkBlock::Feature == cur_block =>
                                    interface_reqrem = VkReqRem::Remove(find_attribute(tag_attrs, "profile").map(|s| registry.append_str(s))),
                                "command"
                                    if VkBlock::Feature == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        feature_buffer.as_mut().unwrap().push_command(registry.append_str(name), &interface_reqrem);
                                    } else {panic!("Could not find feature name")},
                                "enum"
                                    if VkBlock::Feature == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        feature_buffer.as_mut().unwrap().push_enum(registry.append_str(name), &interface_reqrem);
                                    } else {panic!("Could not find feature name")},
                                "type"
                                    if VkBlock::Feature == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        feature_buffer.as_mut().unwrap().push_type(registry.append_str(name), &interface_reqrem);
                                    } else {panic!("Could not find feature name")},

                                "extensions" => cur_block = VkBlock::Extensions,
                                "extension"
                                    if VkBlock::Extensions == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        if let Some(num) = find_attribute(tag_attrs, "number") {
                                            registry.push_extn(extn_buffer).ok();
                                            extn_buffer = Some(VkExtn::new(registry.append_str(name), isize::from_str_radix(num, 10).unwrap()))
                                        } else {panic!("Could not find extension number")}
                                    } else {panic!("Could not find extension name")},
                                "command"
                                    if VkBlock::Extensions == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        extn_buffer.as_mut().unwrap().push_command(registry.append_str(name), &interface_reqrem);
                                    } else {panic!("Could not find extension command name")},
                                "type"
                                    if VkBlock::Extensions == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        extn_buffer.as_mut().unwrap().push_type(registry.append_str(name), &interface_reqrem);
                                    } else {panic!("Could not find extension type name")},
                                "enum"
                                    if VkBlock::Extensions == cur_block =>
                                    if let Some(name) = find_attribute(tag_attrs, "name") {
                                        let name = registry.append_str(name);

                                        if let Some(extends) = find_attribute(tag_attrs, "extends") {
                                            let variant =
                                                if let Some(offset) = find_attribute(tag_attrs, "offset") {
                                                    // Determine enumerant value, as defined in the "Layers & Extensions" appendix of the spec
                                                    let offset = isize::from_str_radix(offset, 10).unwrap();
                                                    let extn_num = extn_buffer.as_ref().unwrap().num;
                                                    let mut value = BASE_VALUE + (extn_num - 1) * RANGE_SIZE + offset;

                                                    if let Some("-") = find_attribute(tag_attrs, "dir") {
                                                        value = -value;
                                                    }

                                                    VkVariant::new_value(name, value)
                                                } else if let Some(value) = find_attribute(tag_attrs, "value") {
                                                    let value = isize::from_str_radix(value, 10).unwrap();
                                                    VkVariant::new_value(name, value)
                                                } else if let Some(bitpos) = find_attribute(tag_attrs, "bitpos") {
                                                    let bitpos = isize::from_str_radix(bitpos, 10).unwrap();
                                                    VkVariant::new_bitpos(name, bitpos)
                                                } else {panic!("Invalid enum extension; missing \"offset\" or \"bitpos\"")};

                                            extn_buffer.as_mut().unwrap().push_enum(
                                                variant,
                                                Some(registry.append_str(extends)),
                                                &interface_reqrem);
                                        } else if let Some(value) = find_attribute(tag_attrs, "value") {
                                            extn_buffer.as_mut().unwrap().push_enum(
                                                VkVariant::new_const(name, registry.append_str(value)),
                                                None, 
                                                &interface_reqrem);
                                        }
                                    } else {panic!("Could not find enum name")},
                                _ => ()
                            },

                        Characters{ref chars, tags: (tag, _)}
                            if VkBlock::Types == cur_block => {
                            let chars = &chars[..];
                            match type_buffer {
                                VkType::Struct{fields: ref mut members, ..} |
                                VkType::Union{variants: ref mut members, ..} =>
                                    match tag {
                                        "member" =>
                                            match chars {
                                                "const" => members.last_mut().unwrap().field_type.make_const(),
                                                "*"     => members.last_mut().unwrap().field_type.make_ptr(),
                                                _ 
                                                    if &chars[0..1] == "[" =>
                                                    match parse_array_index(chars) {
                                                        Some((size, _)) => {members.last_mut().unwrap().field_type.make_array(size);}
                                                        None => panic!(format!("Unexpected characters after name: {}", chars))
                                                    },
                                                _       => ()
                                            },
                                        "type"   => members.last_mut().unwrap().field_type.set_type(registry.append_str(chars)),
                                        "name"   => 
                                            if let Some((size, name_len)) = parse_array_index(chars) {
                                                let member = members.last_mut().unwrap();
                                                member.field_type.make_array(size);
                                                member.set_name(registry.append_str(&chars[..name_len]));
                                            } else {members.last_mut().unwrap().set_name(registry.append_str(chars))},
                                        "enum"   => members.last_mut().unwrap().field_type.set_array_len(registry.append_str(chars)),
                                        _        => ()
                                    },
                                VkType::TypeDef{ref mut typ, 
                                                ref mut name, 
                                                ref mut validity} =>
                                    match tag {
                                        "type" =>
                                            match chars {
                                                "typedef" => *validity ^= NOTYPEDEF,
                                                ";"       => *validity ^= NOSEMICOLON,
                                                _         => *typ = registry.append_str(chars)
                                            },
                                        "name" => *name = registry.append_str(chars),
                                        _ => panic!("Unexpected tag")
                                    },
                                VkType::Handle{ref mut name, 
                                               ref mut validity, 
                                               ref mut dispatchable} =>
                                    match tag {
                                        "type" =>
                                            match chars {
                                                "VK_DEFINE_HANDLE"                  =>  *validity = true,
                                                "VK_DEFINE_NON_DISPATCHABLE_HANDLE" => {*validity = true; *dispatchable = false}
                                                "("                                  |
                                                ")"                                 => (),
                                                _                                   => panic!("Unexpected handle")
                                            },
                                        "name" => *name = registry.append_str(chars),
                                        _ => ()
                                    },
                                VkType::Define{ref mut name} =>
                                    match tag {
                                        "name" => *name = registry.append_str(chars),
                                        "type" => (),
                                        _      => panic!("Unexpected define tag")
                                    },
                                VkType::FuncPointer{ref mut name} =>
                                    match tag {
                                        "name" => *name = registry.append_str(chars),
                                        "type" => (),
                                        _      => panic!("Unexpected define tag")
                                    },
                                _ => ()
                            }
                        }

                        Characters{ref chars, tags: (tag, Some(tag1))}
                            if VkBlock::Commands == cur_block => {
                                let chars = &chars[..];
                                let command_buffer = command_buffer.as_mut().unwrap();

                                if let Some(last_param) = command_buffer.params.last_mut() {
                                    if chars == "const" {
                                        last_param.typ.make_const();
                                    } else if chars == "*" {
                                        last_param.typ.make_ptr();
                                    } else if &chars[0..1] == "[" {
                                        if let Some((len, _)) = parse_array_index(chars) {
                                            last_param.typ.make_array(len)
                                        } else {
                                            panic!("Unexpected characters after array operator")
                                        }
                                    }
                                    match tag1 {
                                        "proto" => panic!("Unexpected proto tag"),
                                        "param" =>
                                            match tag {
                                                "type" => last_param.typ.set_type(registry.append_str(chars)),
                                                "name" => last_param.name = registry.append_str(chars),
                                                _      => panic!("Unexpected tag")
                                            },
                                        _ => ()
                                    }
                                } else if tag1 == "proto" {
                                    match tag {
                                        "type" => 
                                            if chars == "void" {
                                                command_buffer.ret.make_void();
                                            } else {command_buffer.ret.set_type(registry.append_str(chars))},
                                        "name" => command_buffer.name = registry.append_str(chars),
                                        _      => panic!("Unexpected tag")
                                    }
                                }
                            }
                        Characters{..} => ()
                    }
                }

                pop_element_stack(&mut vk_elements);
                popped_to = vk_elements.len();
            }

            XmlEvent::Characters(chars) => {
                let tags = get_tags(&vk_elements);

                unsafe{ vk_elements.push(XmlElement::new_chars(chars, (&*tags.0, tags.1.map(|t| &*t)))) }
            }

            _ => ()
        }
    }

    // The loop doesn't push the last type/command, so that's handled here.
    registry.push_type(type_buffer).ok();
    registry.push_command(command_buffer).ok();
    registry.push_feature(feature_buffer).ok();
    registry.push_extn(extn_buffer).ok();

    unsafe{
        for t in &registry.types {
                match *t.1 {
                    VkType::Struct{name, ref fields}  => {
                        println!("Struct {:?}", &*name);
                        for f in fields {
                            println!("\t{:?}", f);
                        }
                    }

                    VkType::Union{name, ref variants} => {
                        println!("Union {:?}", &*name);
                        for v in variants {
                            println!("\t{:?}", v);
                        }
                    }

                    VkType::Enum{name, ref variants} => {
                        println!("Enum {:?}", &*name);
                        for v in variants {
                            println!("\t{:?}", v);
                        }
                    }

                    VkType::TypeDef{typ, name, validity} =>
                        if validity != 0 {
                            panic!("Invalid typedef")
                        } else {
                            println!("TypeDef {:?} {:?}", &*typ, &*name)
                        },

                    VkType::Handle{name, validity, dispatchable} =>
                        if !validity {
                            panic!("Invalid handle")
                        } else if dispatchable {
                            println!("Handle {:?}", &*name)
                        } else {
                            println!("Non-Dispatchable Handle {:?}", &*name)
                        },

                    VkType::Define{name}      => println!("Define {:?}", &*name),
                    VkType::FuncPointer{name} => println!("FuncPointer {:?}", &*name),

                    VkType::Unhandled => ()
                }
        }

        for c in &registry.commands {
            println!("{:#?}", c.1);
        }

        for f in &registry.features {
            println!("Feature: {:?}", &*f.name);
            for req in &f.require {
                println!("\tRequire: {:?}", req);
            }

            for rem in &f.remove {
                println!("\tRemove: {:?}", rem);
            }
        }

        for e in &registry.extns {
            println!("Extension: {:?}", &*e.name);
            for req in &e.require {
                println!("\tRequire: {:?}", req);
            }

            for rem in &e.remove {
                println!("\tRemove: {:?}", rem);
            }
        }
    }
}

fn to_isize(source: &str) -> Result<isize, ParseIntError> {
    if source.len() < 2 || &source[0..2] != "0x" {
        isize::from_str_radix(source, 10)
    } else {
        isize::from_str_radix(&source[2..], 16)
    }
}

fn find_attribute<'v>(source: &'v Vec<OwnedAttribute>, query: &str) -> Option<&'v str> {
    source.into_iter().skip_while(|attr| &attr.name.local_name != query).next().map(|res| &*res.value)
}

/// Takes a mutable reference to a XmlElement stack, popping the stack of Character elements until
/// it pops a Tag
fn pop_element_stack(vk_elements: &mut Vec<XmlElement>) {
    if let Some(el) = vk_elements.pop() {
        if let XmlElement::Characters{..} = el {
            pop_element_stack(vk_elements)
        }
    } else {panic!("Invalid xml; Unexpected closing tag")}
}

fn get_tags(vk_elements: &Vec<XmlElement>) -> (*const str, Option<*const str>) {
    fn recursive(mut vk_elements: Iter<XmlElement>, mut tags: [Option<*const str>; 2], mut index: usize) -> (*const str, Option<*const str>) {
        match vk_elements.next_back() {
            Some(el) if index < 2 => {
                if let XmlElement::Tag{ref name, ..} = *el {
                    tags[index] = Some(&name[..] as *const str);
                    index += 1;
                }
                recursive(vk_elements, tags, index)
            }
            _                     => (tags[0].unwrap(), tags[1])
        }
    }

    recursive(vk_elements.iter(), [None; 2], 0)
}

/// Takes a string slice and extracts x from [x], as long as [x] is at the end.
///
///
/// Returns (x, length of `chars` without [x]) if "[x]" is detected
///
/// Returns (0, 0) if "[" is detected at end instead of "]"
fn parse_array_index(chars: &str) -> Option<(usize, usize)> {
    let mut chariter = chars.chars().rev();
    match chariter.next().unwrap() {
        // This only occurs when the variable is going to be an array. Now, in most cases it appears
        // *outside* of <name>, but some dumbass decided to have two fields in the entire xml contain it
        // inside. To the person who did that (you know who you are): if you're writing a document designed
        // to be easy to parse, PLEASE keep it consistent. This goes for everyone else as well - think of
        // the person that has to write an interpreter for your goddamn file.
        ']' => {
            // The size of the array
            let mut size = 0;
            for (i, digit) in chariter.clone().take_while(|c| c.is_digit(10)).enumerate() {
                size += digit.to_digit(10).unwrap() as usize * (10usize.pow(i as u32));
            }

            // The length of the actual name, dropping the array length. Is decremented in the iterator below.
            let mut name_len = chars.len()-1;
            match chariter.skip_while(|c| {name_len -= 1; 
                                           c.is_digit(10) | c.is_whitespace()}
                                           ).next() {
                Some('[') => (),
                Some(c)   => panic!(format!("Expected '['; found '{}'", c)),
                None      => panic!("Expected '['; found nothing")
            }
            Some((size, name_len))
        }
        '[' => Some((0, 0)),
        _   => None
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
        tags: (&'a str, Option<&'a str>)
    }
}

impl<'a> XmlElement<'a> {
    fn new_tag(name: OwnedName, attributes: Vec<OwnedAttribute>) -> XmlElement<'a> {
        XmlElement::Tag {
            name: name.local_name,
            attributes: attributes
        }
    }

    fn new_chars(chars: String, tags: (&'a str, Option<&'a str>)) -> XmlElement<'a> {
        XmlElement::Characters {
            chars: chars,
            tags: tags
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum VkBlock {
    Types,
    Enums,
    Commands,
    Extensions,
    Feature,
    None
}

const BASE_VALUE: isize = 1000000000;
const RANGE_SIZE: isize = 1000;