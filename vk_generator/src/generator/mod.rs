use to_option;
use registry::*;
use std::collections::HashMap;
use std::iter::Iterator;
use std::default;

pub struct GenConfig {
    remove_type_prefix: bool,
    remove_command_prefix: bool,
    remove_variant_prefix: bool,
    style_rust: bool
}

impl default::Default for GenConfig {
    fn default() -> GenConfig{
        GenConfig {
            remove_type_prefix: true,
            remove_command_prefix: true,
            remove_variant_prefix: true,
            style_rust: false
        }
    }
}

struct GenPreproc<'a> {
    types: HashMap<&'a str, VkType>,
    registry: &'a VkRegistry<'a>,
    config: GenConfig
}

impl<'a> GenPreproc<'a> {
    fn new(registry: &'a VkRegistry<'a>, version: VkVersion, extensions: &[&str], config: GenConfig) -> GenPreproc<'a> {
        let mut gen = GenPreproc {
            types: HashMap::with_capacity(registry.types().len()),
            registry: registry,
            config: config
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
            },

            Type{name, ..} |
            ApiConst{name, ..} => {
                let name = unsafe{ &*name };
                if name != "vk_platform" {
                    self.add_type(name);
                }
            },

            ConstDef{name, value, ..} => {
                let name = unsafe{ &*name };
                let value = unsafe{ &*value };
                if self.types.insert(name, VkType::new_const(name, value)).is_some() {
                    panic!("Overlapping constants!");
                }
            },

            ExtnEnum{extends, ref variant, ..} => {
                let extends = unsafe{ &*extends };
                if let VkType::Enum{ref mut variants, ..} = *self.types.get_mut(extends).unwrap() {
                    variants.push(variant.clone());
                }
            }
        }
    }

    fn add_type(&mut self, name: &'a str) {
        self.types.entry(name).or_insert(self.registry.types().get(name).unwrap().clone());
    }

    fn add_type_recurse(&mut self, type_iterator: &mut Iterator<Item=&VkElType>) {
        for typ in type_iterator {
            use registry::VkType::*;

            if let Some(type_ptr) = typ.type_ptr() {
                let type_ptr = unsafe{ &*type_ptr };
                self.types.entry(type_ptr).or_insert(self.registry.types().get(type_ptr).unwrap().clone());

                match *self.registry.types().get(type_ptr).unwrap() {
                    Struct{fields: ref members, ..} |
                    Union{variants: ref members, ..} => self.add_type_recurse(&mut members.into_iter().map(|m| &m.field_type)),
                    TypeDef{requires, ..} => 
                        if let Some(requires) = to_option(requires) {
                            self.types.entry(requires).or_insert(self.registry.types().get(requires).unwrap().clone());
                        },
                    _ => ()
                }
            }
        }
    }
}

impl<'a> VkRegistry<'a> {
    pub fn gen_global(&self, version: VkVersion, extensions: &[&str], config: GenConfig) {
        let generator = GenPreproc::new(self, version, extensions, config);

        for typ in generator.types.values() {
            unsafe{
                match *typ {
                    VkType::Struct{name, ..}  => {
                        println!("Struct {:?}", &*name);
                    }

                    VkType::Union{name, ..} => {
                        println!("Union {:?}", &*name);
                    }

                    VkType::Enum{name, ..} => {
                        println!("Enum {:?}", &*name);
                    }

                    VkType::TypeDef{typ, name, requires, validity} =>
                        if validity != 0 {
                            panic!("Invalid typedef")
                        } else {
                            println!("TypeDef {:?} {:?} {:?}", &*typ, &*name, to_option(requires))
                        },

                    VkType::Handle{name, validity, dispatchable} =>
                        if !validity {
                            panic!("Invalid handle")
                        } else if dispatchable {
                            println!("Handle {:?}", &*name)
                        } else {
                            println!("Non-Dispatchable Handle {:?}", &*name)
                        },

                    VkType::ApiConst{name, value} =>
                        println!("API Const: {} {}", &*name, &*value),

                    VkType::Define{name}      => println!("Define {:?}", &*name),
                    VkType::FuncPointer{name} => println!("FuncPointer {:?}", &*name),
                    VkType::ExternType{name, requires}  => println!("ExternType {:?} {:?}", &*name, &*requires),

                    VkType::Unhandled => ()
                }
            }
        }
        println!("{}", generator.types.len());
    }
}

pub trait GenRegistry {
    fn features(&self)   -> &HashMap<VkVersion, VkFeature>;
    fn types(&self)      -> &HashMap<&str, VkType>;
    fn commands(&self)   -> &HashMap<&str, VkCommand>;
    fn extns(&self) -> &HashMap<&str, VkExtn>;
}