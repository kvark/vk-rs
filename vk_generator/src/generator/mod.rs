use to_option;
use registry::*;
use std::collections::HashMap;
use std::iter::Iterator;
use std::default;
use std::fmt::Write;

use boolinator::Boolinator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GenConfig {
    pub remove_type_prefix: bool,
    pub remove_command_prefix: bool,
    pub remove_enum_prefix: bool,
    pub remove_bitmask_prefix: bool,
    pub snake_case_commands: bool,
    pub camel_case_enums: bool
}

impl GenConfig {
    fn modifies_signatures(&self) -> bool {
        self.remove_command_prefix ||
        self.snake_case_commands ||
        self.camel_case_enums
    }
}

impl default::Default for GenConfig {
    fn default() -> GenConfig{
        GenConfig {
            remove_type_prefix: true,
            remove_command_prefix: true,
            remove_enum_prefix: true,
            remove_bitmask_prefix: true,
            snake_case_commands: true,
            camel_case_enums: true
        }
    }
}

struct GenPreproc<'a> {
    types: HashMap<&'a str, VkType>,
    type_ord: Vec<&'a str>,
    commands: Vec<VkCommand>,
    registry: &'a VkRegistry<'a>,
    config: GenConfig,
    string_buffer: Option<String>
}

impl<'a> GenPreproc<'a> {
    fn new(registry: &'a VkRegistry<'a>, version: VkVersion, extensions: &[&str], config: GenConfig) -> GenPreproc<'a> {
        let mut gen = GenPreproc {
            string_buffer: config.modifies_signatures().as_some(String::with_capacity(registry.buffer_cap())),
            types: HashMap::with_capacity(registry.types().len()),
            type_ord: Vec::with_capacity(registry.types().len()),
            commands: Vec::with_capacity(registry.commands().len()),
            registry: registry,
            config: config,
        };

        let feature = gen.registry.features().get(&version).unwrap();

        for req in &feature.require {
            gen.add_interface(req);
        }

        for e in extensions {
            let ex = gen.registry.extns().get(e).unwrap();
            for req in &ex.require {
                gen.add_interface(req);
            }
        }

        gen
    }

    fn add_interface(&mut self, interface: &VkInterface) {
        use registry::VkInterface::*;
        match *interface {
            Command{name, ..} => {
                let name = unsafe{ &*name };
                let command = self.registry.commands().get(name).unwrap();

                self.add_type_recurse(&mut (&command.params).into_iter().map(|p| &p.typ).chain(Some(&command.ret)));
                self.add_command(command.clone());
            },

            Type{name, ..}      |
            ApiConst{name, ..} => {
                let name = unsafe{ &*name };
                if name != "vk_platform" {
                    self.add_type(name);
                }
            },

            ConstDef{name, value, ..} => {
                let name = unsafe{ &*name };
                let value = unsafe{ &*value };
                self.insert_type(name, VkType::new_const(name, value));
            },

            ExtnEnum{extends, ref variant, ..} => {
                let extends = unsafe{ &*extends };
                let mut variant = variant.clone();

                match *self.types.get(extends).unwrap() {
                    VkType::Enum{name, ..} => self.process_enum_variant(&mut variant, name),
                    VkType::Bitmask{..}    => self.process_bitmask_variant(&mut variant),
                    _                      => ()
                }
                match *self.types.get_mut(extends).unwrap() {
                    VkType::Enum{ref mut variants, ..}     |
                    VkType::Bitmask{ref mut variants, ..} => variants.push(variant),
                    _                                     => ()
                }
            }
        }
    }

    fn add_type(&mut self, name: &'a str) {
        self.insert_type(name, self.registry.types().get(name).unwrap().clone());
    }

    fn add_type_recurse(&mut self, type_iterator: &mut Iterator<Item=&VkElType>) {
        for typ in type_iterator {
            use registry::VkType::*;

            if let Some(type_ptr) = typ.type_ptr() {
                let type_ptr = unsafe{ &*type_ptr };
                self.add_type(type_ptr);

                match *self.registry.types().get(type_ptr).unwrap() {
                    Struct{fields: ref members, ..} |
                    Union{variants: ref members, ..} => self.add_type_recurse(&mut members.into_iter().map(|m| &m.field_type)),
                    TypeDef{typ, requires, ..} => {
                        self.add_type(unsafe{ &*typ });
                        if let Some(requires) = to_option(requires) {
                            self.add_type(requires);
                        }
                    }
                    _ => ()
                }
            }
        }
    }

    fn insert_type(&mut self, key: &'a str, mut typ: VkType) {
        use std::collections::hash_map::Entry;

        let new_name = self.process_type_ident(typ.name().unwrap());
        typ.set_name(new_name).ok();

        match typ {
            VkType::TypeDef{typ: ref mut typedef_type, ref mut requires, ..} => {
                *typedef_type = self.process_type_ident(*typedef_type);

                if let Some(req) = to_option(*requires) {
                    *requires = self.process_type_ident(req);
                }
            }

            VkType::Enum{ref mut variants, name: enum_name} =>
                for v in variants.iter_mut() {
                    self.process_enum_variant(v, enum_name);
                },

            VkType::Bitmask{ref mut variants, ..} =>
                for v in variants.iter_mut() {
                    self.process_bitmask_variant(v);
                },
            _ => ()
        }

        if let Entry::Vacant(ven) = self.types.entry(key) {
            ven.insert(typ);
            self.type_ord.push(key);
        }
    }

    fn process_type_ident(&self, ident: *const str) -> *const str {
        let mut ident = unsafe{ &*ident };

        if self.config.remove_type_prefix {
            if let Some(0) = ident.find("Vk") {
                ident = &ident[2..];
            }
        }

        ident
    }

    fn process_bitmask_variant(&self, variant: &mut VkVariant) {
        let mut name = unsafe{ &*variant.name() };

        if self.config.remove_bitmask_prefix {
            if let Some(0) = name.find("VK_") {
                name = &name[3..];
            }
        }
        variant.set_name(name);
    }

    fn process_enum_variant(&mut self, variant: &mut VkVariant, enum_name: *const str) {
        let enum_name = unsafe{ &*enum_name };
        if self.config.remove_enum_prefix {
            let name_parts: Vec<_> = enum_name
                                        .char_indices()
                                        .filter_map( |(i, c)| (c.is_uppercase()).as_some(i) )
                                        .chain(Some(enum_name.len()).into_iter())
                                        .peek_next()
                                        .map( |(s, e)| enum_name[s..e].to_uppercase() )
                                        .collect();

            let vn = unsafe{ &*variant.name() };
            let mut index = if self.config.remove_type_prefix {3} else {0};
            'na: for n in &name_parts {
                if let Some(i) = (&vn[index..]).find(&n[..]) {
                    index += i + n.len() + 1;
                } else {break 'na}
            }
            variant.set_name(&vn[index..]);
        }

        if self.config.camel_case_enums {
            unsafe{ 
                let vn = &*variant.name();
                let vn_new = self.append_char_func(
                    |s| {
                        let mut is_uppercase = true;
                        for c in vn.chars() {
                            if c == '_' {
                                is_uppercase = true;
                            } else if is_uppercase {
                                s.push(c);
                                is_uppercase = false;
                            } else {
                                s.push(c.to_lowercase().next().unwrap())
                            }
                        }
                    }
                );
                variant.set_name(vn_new);
            }
        }
    }

    fn add_command(&mut self, mut command: VkCommand) {
        for typ in command.params.iter_mut().map(|p| &mut p.typ).chain(Some(&mut command.ret).into_iter()) {
            if let Some(mut type_ptr) = typ.type_ptr() {
                type_ptr = self.process_type_ident(type_ptr);
                typ.set_type(type_ptr);
            }
        }

        command.name = self.process_command_ident(command.name);

        self.commands.push(command);
    }

    fn process_command_ident(&mut self, ident: *const str) -> *const str {
        let mut ident = unsafe{ &*ident };

        if self.config.remove_command_prefix {
            if let Some(0) = ident.find("vk") {
                ident = unsafe{&*self.append_char_func(
                    |s| {
                        let mut chars = ident.chars().skip(2);
                        for c in chars.next().unwrap().to_lowercase().chain(chars) {
                            s.push(c);
                        }
                    }
                )};
            }
        }

        if self.config.snake_case_commands {
            ident = unsafe{&*self.append_char_func(
                |s| {
                    let mut last_uppercase = false;
                    for c in ident.chars() {
                        if c.is_uppercase() {
                            if !last_uppercase {s.push('_')}
                            last_uppercase = true;
                        } else {last_uppercase = false}

                        s.push(c.to_lowercase().next().unwrap());
                    }
                }
            )};
        }

        ident
    }

    unsafe fn append_char_func<F: Fn(&mut String)>(&mut self, processor: F) -> *const str {
        use std::{slice, str};
        let string_buffer = self.string_buffer.as_mut().unwrap();

        let prepushcap = string_buffer.capacity();
        let prepushlen = string_buffer.len();
        // We want to have all of the string in one block of memory in order to save heap allocation time. 
        processor(string_buffer);

        if prepushcap != string_buffer.capacity() {
            panic!("Allocation detected in string buffer")
        }

        let ptr = string_buffer.as_ptr().offset(prepushlen as isize);
        str::from_utf8_unchecked(slice::from_raw_parts(ptr, string_buffer.len()-prepushlen)) as *const str
    }
}

struct GenTypes {
    structs:  String,
    unions:   String,
    enums:    String,
    bitmasks: String,
    handles:  String,
    typedefs: String,
    consts:   String,
    externs:  String
}

impl GenTypes {
    fn new(processed: &GenPreproc) -> GenTypes {
        let mut gen_types = GenTypes {
            structs:  String::with_capacity(2usize.pow(17)),
            unions:   String::with_capacity(2usize.pow(13)),//take that you superstitious bastards 
            enums:    String::with_capacity(2usize.pow(15)),
            bitmasks: String::with_capacity(2usize.pow(15)),
            handles:  String::with_capacity(2usize.pow(12)),
            typedefs: String::with_capacity(2usize.pow(13)),
            consts:   String::with_capacity(2usize.pow(10)),
            externs:  String::with_capacity(2usize.pow(10))
        };

        for t in processed.type_ord.iter().map(|k| processed.types.get(k).unwrap()) {
            use registry::VkType::*;
            use registry::VkElType::*;
            use registry::VkVariant::*;

            match *t {
                Struct{name, ref fields} => {
                    let structs = &mut gen_types.structs;
                    writeln!(structs, "#[repr(C)]\n#[derive(Debug, Clone)]\npub struct {} {{", unsafe{ &*name }).unwrap();

                    for f in fields { unsafe {
                        match f.field_type {
                            Var(ident) => writeln!(structs, "    {}: {},", &*f.field_name, &*ident),
                            ConstPtr(ident, count) => {
                                write!(structs, "    {}: ", &*f.field_name).unwrap();
                                for _ in 0..count {
                                    write!(structs, "*const ").unwrap();
                                }
                                writeln!(structs, "{},", &*ident)
                            }
                            MutPtr(ident, count) => {
                                write!(structs, "    {}: ", &*f.field_name).unwrap();
                                for _ in 0..count {
                                    write!(structs, "*mut ").unwrap();
                                }
                                writeln!(structs, "{},", &*ident)
                            }
                            MutArray(ident, count)    => writeln!(structs, "    {}: {}[{}],", &*f.field_name, &*ident, count),
                            MutArrayEnum(ident, cons) => writeln!(structs, "    {}: {}[{}],", &*f.field_name, &*ident, &*cons),

                            ConstArray(_, _)      |
                            ConstArrayEnum(_, _) => panic!("Unexpected const array in struct"),
                            Const(_)             => panic!("Unexpected const {}", &*name),
                            Void                 => panic!("Unexpected void"),
                            Unknown              => panic!("Unexpected unknown")
                        }.unwrap();
                    }}

                    structs.push_str("}\n\n");
                }

                // Unions are currently a pain in the ass to do, as Rust does not have a stable implementation.     |
                // What they do have, however, is an approved RFC that is currently being implemented. Until those  |
                // become reality the unions currently present in Vulkan are simply going to be hard-coded into the |
                // generator with a fairly shitty, although functional, implementation.                             |
                Union{..} => (),

                Enum{name, ref variants} => {
                    let enums = &mut gen_types.enums;
                    writeln!(enums, "#[repr(C)]\n#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]\npub enum {} {{", unsafe{ &*name }).unwrap();

                    for v in variants {unsafe {
                        match *v {
                            Value{name, value} => writeln!(enums, "    {} = {},", &*name, value),
                            Bitpos{..}         => panic!("Found bitpos in non-biflags enum")
                        }.unwrap();
                    }}

                    enums.push_str("}\n\n");
                }

                Bitmask{name, ref variants} => {
                    let bitmasks = &mut gen_types.bitmasks;
                    writeln!(bitmasks, include_str!("bitmask_struct.rs"), unsafe{ &*name }).unwrap();

                    let mut all_bits = 0;
                    for v in variants {unsafe {
                        let bits = 
                            match *v {
                                Value{value, ..}   => value,
                                Bitpos{bitpos, ..} => 2isize.pow(bitpos)
                            };
                        writeln!(bitmasks, "pub const {0}: {1} = {1} {{flags: 0b{2:b}}};", &*v.name(), &*name, bits).unwrap();
                        all_bits |= bits;
                    }}

                    writeln!(bitmasks, include_str!("bitmask_impl.rs"), unsafe{ &*name }, all_bits).unwrap();
                }
                _ => ()
            }
        }

        println!("{}", &gen_types.structs);
        println!("{}", &gen_types.enums);
        println!("{}", &gen_types.bitmasks);
        gen_types
    }
}


impl<'a> VkRegistry<'a> {
    pub fn gen_global(&self, version: VkVersion, extensions: &[&str], config: GenConfig) {
        let generator = GenPreproc::new(self, version, extensions, config);
        GenTypes::new(&generator);

        // for typ in generator.type_ord.iter().map(|k| generator.types.get(k).unwrap()) {
        //     unsafe{
        //         match *typ {
        //             VkType::Struct{name, ..}  => {
        //                 println!("Struct {:?}", &*name);
        //             }

        //             VkType::Union{name, ..} => {
        //                 println!("Union {:?}", &*name);
        //             }

        //             VkType::Enum{name, ..} => {
        //                 println!("Enum {:?}", &*name);
        //             }

        //             VkType::TypeDef{typ, name, requires, validity} =>
        //                 if validity != 0 {
        //                     panic!("Invalid typedef")
        //                 } else {
        //                     println!("TypeDef {:?} {:?} {:?}", &*typ, &*name, to_option(requires))
        //                 },

        //             VkType::Handle{name, validity, dispatchable} =>
        //                 if !validity {
        //                     panic!("Invalid handle")
        //                 } else if dispatchable {
        //                     println!("Handle {:?}", &*name)
        //                 } else {
        //                     println!("Non-Dispatchable Handle {:?}", &*name)
        //                 },

        //             VkType::ApiConst{name, value} =>
        //                 println!("API Const: {} {}", &*name, &*value),

        //             VkType::Define{name}      => println!("Define {:?}", &*name),
        //             VkType::FuncPointer{name} => println!("FuncPointer {:?}", &*name),
        //             VkType::ExternType{name, requires}  => println!("ExternType {:?} {:?}", &*name, &*requires),

        //             VkType::Unhandled => ()
        //         }
        //     }
        // }
        // println!("{}", generator.types.len());

        // for command in generator.commands {
        //     println!("{:#?}", command);
        // }
    }
}

pub trait GenRegistry {
    fn features(&self)   -> &HashMap<VkVersion, VkFeature>;
    fn types(&self)      -> &HashMap<&str, VkType>;
    fn commands(&self)   -> &HashMap<&str, VkCommand>;
    fn extns(&self)      -> &HashMap<&str, VkExtn>;
    fn buffer_cap(&self) -> usize;
}

struct PeekNext<I: Iterator> {
    iter: I,
    peeked: Option<I::Item>
}

impl<I: Iterator> Iterator for PeekNext<I> where I::Item: Copy {
    type Item = (I::Item, I::Item);
    fn next(&mut self) -> Option<Self::Item> {
        let cur = 
            match self.peeked {
                Some(_) => self.peeked.take(),
                None    => self.iter.next()
            };
        
        if let Some(cur) = cur {
            self.peeked = self.iter.next();
            match self.peeked {
                Some(next) => Some((cur, next)),
                None       => None
            }
        } else {None}
    }
}

trait PeekNextCreate where Self: Sized + Iterator {
    fn peek_next(self) -> PeekNext<Self>;
}

impl<I: Iterator> PeekNextCreate for I {
    fn peek_next(self) -> PeekNext<I> {
        PeekNext {
            iter: self,
            peeked: None
        }
    }
}