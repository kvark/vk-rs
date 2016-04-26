use to_option;
use registry::*;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;
use std::default;
use std::fmt::Write;

use boolinator::Boolinator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GenConfig<'a> {
    pub remove_type_prefix: bool,
    pub remove_command_prefix: bool,
    pub remove_enum_padding: bool,
    pub remove_bitmask_prefix: bool,
    pub snake_case_commands: bool,
    pub camel_case_enums: bool,
    pub camel_case_members: bool,

    pub wrap_bitmasks: bool,
    pub use_libc_types: bool,
    pub extern_type_overrides: &'a [(&'a str, &'a str)]
}

impl<'a> GenConfig<'a> {
    fn modifies_signatures(&self) -> bool {
        self.remove_command_prefix ||
        self.snake_case_commands ||
        self.camel_case_enums
    }
}

impl<'a> default::Default for GenConfig<'a> {
    fn default() -> GenConfig<'a> {
        GenConfig {
            remove_type_prefix: true,
            remove_command_prefix: true,
            remove_enum_padding: true,
            remove_bitmask_prefix: true,
            snake_case_commands: true,
            camel_case_enums: true,
            camel_case_members: true,

            wrap_bitmasks: true,
            use_libc_types: false,
            extern_type_overrides: &[]
        }
    }
}

struct GenPreproc<'a, 'b> {
    types: HashMap<&'a str, VkType>,
    type_ord: Vec<&'a str>,
    const_types: HashMap<&'a str, ConstType>,
    /// A set of all types that can't have Debug and Clone implementations derived. This occurs when
    /// the type contains an array, and as such can't derive Clone (because arrays only implement Clone
    /// if the types they contain are Copy, which all of the generated types aren't) and Debug.
    custom_impls: HashSet<&'a str>,
    commands: Vec<VkCommand>,
    registry: &'a VkRegistry<'a>,
    config: GenConfig<'b>,
    string_buffer: Option<String>
}

impl<'a, 'b> GenPreproc<'a, 'b> {
    fn new(registry: &'a VkRegistry<'a>, version: VkVersion, extensions: &[&str], config: GenConfig<'b>) -> GenPreproc<'a, 'b> {
        let mut gen = GenPreproc {
            string_buffer: config.modifies_signatures().as_some(String::with_capacity(registry.buffer_cap())),
            types: HashMap::with_capacity(registry.types().len()),
            type_ord: Vec::with_capacity(registry.types().len()),
            const_types: HashMap::with_capacity(registry.core_consts().len()),
            custom_impls: HashSet::with_capacity(32),
            commands: Vec::with_capacity(registry.commands().len()),
            registry: registry,
            config: config,
        };

        let feature = gen.registry.features().get(&version).unwrap();

        for c in registry.core_consts() {
            gen.add_type(c);
        }

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
                let mut command = self.registry.commands().get(name).unwrap().clone();

                for p in command.params.iter_mut() {
                    self.add_type_recurse(&mut p.typ);
                }
                self.add_type_recurse(&mut command.ret);

                self.add_command(command);
            },

            ApiConst{name, ..} => {self.add_type(unsafe{ &*name });},
            Type{name, ..}     => {
                let name = unsafe{ &*name };
                if name != "vk_platform" {
                    self.add_type_recurse(&mut VkElType::Var(name));
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

    fn add_type_recurse(&mut self, typ: &mut VkElType) {
        use registry::VkType::*;

        if let Some(type_ptr) = typ.type_ptr() {
            typ.set_type(self.process_type_ident(type_ptr));
            let type_ptr = unsafe{ &*type_ptr };
            match *typ {
                VkElType::ConstArrayEnum(_, c) |
                VkElType::MutArrayEnum(_, c)  => self.const_types.insert(unsafe{ &*c }, ConstType::USize),
                _ => None
            };

            if let Some(t) = self.add_type(type_ptr) {
                unsafe {
                    let mut custom_impl = false;

                    match *t {
                        Struct{fields: ref mut members, ..} |
                        Union{variants: ref mut members, ..} =>
                            for m in members.iter_mut() {
                                m.field_name = self.process_member_name(m.field_name);
                                match m.field_type {
                                    VkElType::ConstArray(_, _)     |
                                    VkElType::MutArray(_, _)       |
                                    VkElType::ConstArrayEnum(_, _) |
                                    VkElType::MutArrayEnum(_, _)  => custom_impl = true,
                                    _ => ()
                                };

                                self.add_type_recurse(&mut m.field_type);
                            },
                        TypeDef{..} => 
                            if let TypeDef{typ, requires, ..} = *self.registry.types().get(type_ptr).unwrap() {
                                self.add_type(&*typ);
                                if let Some(requires) = to_option(requires) {
                                    self.add_type(requires);
                                }
                            } else {panic!("Registry type does not match up with modified type")},
                        _ => ()
                    }

                    if custom_impl {
                        self.custom_impls.insert(&*(*t).name().unwrap());
                    }
                }
            }
        }
    }

    fn process_member_name(&mut self, name: *const str) -> *const str {
        let mut name = unsafe{ &*name };

        if self.config.camel_case_members { unsafe {
            name = &*self.append_char_func(|s| {
                let mut cl = ' ';
                for c in name.chars() {
                    if cl.is_lowercase() && c.is_uppercase() {
                        s.push('_')
                    }
                    s.push(c.to_lowercase().next().unwrap());
                    cl = c;
                }
            });
        }}

        if "type" == name {
            "typ"
        } else {name}
    }

    fn add_type(&mut self, name: &'a str) -> Option<*mut VkType> {
        self.insert_type(name, self.registry.types().get(name).unwrap().clone())
    }

    fn insert_type(&mut self, key: &'a str, mut typ: VkType) -> Option<*mut VkType> {
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
            self.type_ord.push(key);
            Some(ven.insert(typ))
        } else {None}
    }

    fn process_type_ident(&self, ident: *const str) -> *const str {
        let mut ident = unsafe{ &*ident };

        match ident {
            "void"      => ident = "c_void",
            "char"      => ident = "c_char",
            "double"    => ident = "c_double",
            "float"     => ident = "c_float",
            "int"       => ident = "c_int",
            "long"      => ident = "c_long",
            "longlong"  => ident = "c_longlong",
            "schar"     => ident = "c_schar",
            "short"     => ident = "c_short",
            "uchar"     => ident = "c_uchar",
            "uint"      => ident = "c_uint",
            "ulong"     => ident = "c_ulong",
            "ulonglong" => ident = "c_ulonglong",
            "ushort"    => ident = "c_ushort",
            _ => ()
        }

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
        if self.config.remove_enum_padding {
            let name_parts: Vec<_> = enum_name
                                        .char_indices()
                                        .filter_map( |(i, c)| (c.is_uppercase()).as_some(i) )
                                        .chain(Some(enum_name.len()).into_iter())
                                        .peek_next()
                                        .map( |(s, e)| enum_name[s..e].to_uppercase() )
                                        .collect();
            let vn = unsafe{ &*variant.name() };
            // These are the indicies of various parts of the variant name, shown by example:
            // If the enum is named `VkImageType`, and has the variant `VK_IMAGE_TYPE_1D`:
            // 
            // VK_IMAGE_TYPE_1D
            //               ^ `start` is this index
            // VK_IMAGE_TYPE_1D
            //          ^ `old_start` is this index
            //
            // `old_start` is used instead of `start` if `start` points to an index where it could not create a
            // valid identifier (i.e. `1D` is not a valid ident, but `Type1D` or `TYPE_1D` is)
            let mut old_start = 0;
            let mut start = if self.config.remove_type_prefix {3} else {0};

            'na: for n in &name_parts {
                if vn[start..].starts_with(n) {
                    old_start = start;
                    start += n.len() + 1;
                } else {break 'na}
            }

            let mut end = vn.len();
            'ne: for n in name_parts.iter().rev() {
                if vn[..end].ends_with(n) {
                    end -= n.len();
                } else if vn[..end].ends_with('_') {
                    end -= 1;
                } else {break 'ne}
            }

            if !vn[start..end].chars().next().unwrap().is_digit(10) {
                variant.set_name(&vn[start..end]);
            } else {variant.set_name(&vn[old_start..])}
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
                    let name = unsafe{ &*name };
                    let structs = &mut gen_types.structs;

                    if !processed.custom_impls.contains(name) {
                        writeln!(structs, "#[derive(Debug, Clone)]").unwrap();
                    }
                    writeln!(structs, "#[repr(C)]\npub struct {} {{", name).unwrap();

                    for f in fields { unsafe {
                        write!(structs, "    pub ").unwrap();
                        match f.field_type {
                            Var(ident) => writeln!(structs, "{}: {},", &*f.field_name, &*ident),
                            ConstPtr(ident, count) => {
                                write!(structs, "{}: ", &*f.field_name).unwrap();
                                for _ in 0..count {
                                    write!(structs, "*const ").unwrap();
                                }
                                writeln!(structs, "{},", &*ident)
                            }
                            MutPtr(ident, count) => {
                                write!(structs, "{}: ", &*f.field_name).unwrap();
                                for _ in 0..count {
                                    write!(structs, "*mut ").unwrap();
                                }
                                writeln!(structs, "{},", &*ident)
                            }
                            MutArray(ident, count)    => writeln!(structs, "{}: [{}; {}],", &*f.field_name, &*ident, count),
                            MutArrayEnum(ident, cons) => writeln!(structs, "{}: [{}; {}],", &*f.field_name, &*ident, &*cons),

                            ConstArray(_, _)      |
                            ConstArrayEnum(_, _) => panic!("Unexpected const array in struct"),
                            Const(_)             => panic!("Unexpected const {}", name),
                            Void                 => panic!("Unexpected void"),
                            Unknown              => panic!("Unexpected unknown")
                        }.unwrap();
                    }}
                    structs.push_str("}\n\n");

                    if processed.custom_impls.contains(name) {
                        // Write `Clone` implementation
                        writeln!(structs, include_str!("custom_impl_clone.rs"), name).unwrap();
                        for f in fields {unsafe{
                            write!(structs, "            ").unwrap();
                            let n = &*f.field_name;

                            match f.field_type {
                                MutArray(_, s)        |
                                ConstArray(_, s)     => writeln!(structs, include_str!("clone_array.rs"), n, s),
                                MutArrayEnum(_, s)    |
                                ConstArrayEnum(_, s) => writeln!(structs, include_str!("clone_array.rs"), n, &*s),
                                _                    => writeln!(structs, "{0}: self.{0}.clone(),", n)
                            }.unwrap()
                        }}
                        write!(structs, "        }}\n    }}\n}}\n\n").unwrap();

                        // Write `Debug` implementation
                        writeln!(structs, include_str!("custom_impl_debug.rs"), name).unwrap();
                        for f in fields {unsafe{
                            write!(structs, "           ").unwrap();
                            let n = &* f.field_name;

                            match f.field_type {
                                MutArray(_, _)        |
                                ConstArray(_, _)      |
                                MutArrayEnum(_, _)    |
                                ConstArrayEnum(_, _) => writeln!(structs, ".field(\"{0}\", &&self.{0}[..])", n).unwrap(),
                                _                    => writeln!(structs, ".field(\"{0}\", &self.{0})", n).unwrap()
                            }
                        }}
                        write!(structs, "            .finish()\n    }}\n}}\n\n").unwrap();
                    }
                }

                // Unions are currently a pain in the ass to do, as Rust does not have a stable implementation.     |
                // What they do have, however, is an approved RFC that is currently being implemented. Until those  |
                // become reality the unions currently present in Vulkan are simply going to be hard-coded into the |
                // generator with a fairly shitty, although functional, implementation.                             |
                Union{..} => (),

                Enum{name, ref variants} => {
                    let enums = &mut gen_types.enums;
                    let name = unsafe{ &*name };
                    writeln!(enums, "#[repr(C)]\n#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]\npub enum {} {{", name).unwrap();

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
                    let name = unsafe{ &*name };
                    let flags_name = unsafe{ &*processed.types.get("VkFlags").unwrap().name().unwrap() };

                    if processed.config.wrap_bitmasks {
                        writeln!(bitmasks, include_str!("bitmask_struct.rs"), name, flags_name).unwrap();

                        let mut all_bits = 0;
                        for v in variants {unsafe {
                            let bits = 
                                match *v {
                                    Value{value, ..}   => value,
                                    Bitpos{bitpos, ..} => 2isize.pow(bitpos)
                                };
                            writeln!(bitmasks, "pub const {0}: {1} = {1} {{flags: 0b{2:b}}};", &*v.name(), name, bits).unwrap();
                            all_bits |= bits;
                        }}

                        writeln!(bitmasks, include_str!("bitmask_impl.rs"), name, all_bits, flags_name).unwrap();
                    } else {
                        writeln!(bitmasks, "pub type {} = {};", name, flags_name).unwrap();

                        for v in variants {unsafe {
                            let bits = 
                                match *v {
                                    Value{value, ..}   => value,
                                    Bitpos{bitpos, ..} => 2isize.pow(bitpos)
                                };
                            writeln!(bitmasks, "pub const {0}: {1} = 0b{2:b};", &*v.name(), name, bits).unwrap();
                        }}
                        bitmasks.push('\n');
                    }
                }

                Handle{name, dispatchable, ..} => {
                    let handles = &mut gen_types.handles;
                    let name = unsafe{ &*name };
                    if dispatchable {
                        writeln!(handles, include_str!("handle_dispatchable.rs"), name).unwrap();
                    } else {
                        writeln!(handles, include_str!("handle_nondispatchable.rs"), name).unwrap();
                    }
                }

                TypeDef{name, typ, ..} => {
                    writeln!(gen_types.typedefs, "pub type {} = {};", unsafe{ &*name }, unsafe{ &*typ }).unwrap();
                }

                ApiConst{name, value} => {
                    use self::ConstType::*;

                    let consts = &mut gen_types.consts;
                    let (name, value) = unsafe{ (&*name, (&*value).trim()) };
                    let mut typ = processed.const_types.get(name).map(|t| *t).unwrap_or(Unknown);
                    let mut slice_indices = (0, value.len());

                    for (b, c) in value.char_indices() {
                        match c {
                            ')'                    => (),
                            '(' if typ != Str      => slice_indices.0 = b+1,
                            'U' if typ == Unsigned => slice_indices.1 = b,
                            'U' if typ != Str      => panic!("Unexpected U in {:?} {}; {}", typ, name, c),
                            'L' if typ == Unsigned => typ = ULong,
                            'L' if typ == ULong    => typ = ULongLong,
                            '"'                    => typ = Str,
                            '.' if typ == Unsigned => typ = Float,
                            'f' if typ == Float    => slice_indices.1 = b,
                            _   if 
                                 typ == Unknown &&
                                 c.is_digit(10)    => typ = Unsigned,
                            _   if
                                 (typ == Unsigned  ||
                                  typ == ULongLong ||
                                  typ == USize     ||
                                  typ == Float)    &&
                                 !c.is_digit(10)   => panic!("Unexpected non-digit in {:?} {}; {}", typ, name, c),
                            _ => ()
                        }
                    }

                    let sliced_value = &value[slice_indices.0..slice_indices.1];
                    match typ {
                        Unsigned  => write!(consts, "pub const {}: uint32_t = ", name),
                        ULong     => write!(consts, "pub const {}: c_ulong = ", name),
                        ULongLong => write!(consts, "pub const {}: c_ulonglong =", name),
                        USize     => write!(consts, "pub const {}: size_t = ", name),
                        Float     => writeln!(consts, "pub const {}: c_float = {};", name, sliced_value),
                        Str       => writeln!(consts, "pub const {}: &'static str = {};", name, sliced_value),
                        Unknown   => panic!("Unknown const class")
                    }.unwrap();

                    match typ {
                        Unsigned   |
                        ULong      |
                        ULongLong  |
                        USize     =>
                            if '~' == sliced_value.chars().next().unwrap() {
                                writeln!(consts, "!{};", &sliced_value[1..])
                            } else {writeln!(consts, "{};", sliced_value)}.unwrap(),
                        _ => ()
                    }
                }

                ExternType{name, requires} => {
                    let externs = &mut gen_types.externs;
                    let (name, requires) = unsafe{ (&*name, &*requires) };
                    if let Some(over) = processed.config.extern_type_overrides.iter().find(|o| o.0 == name) {
                        writeln!(externs, "pub type {} = ::{};", name, over.1).unwrap();
                    } else if "vk_platform" == requires {
                        if processed.config.use_libc_types {
                            writeln!(externs, "use libc::{};", name).unwrap();
                        } else {
                            let typ =
                                match name {
                                    "c_void" => "()",
                                    // Following taken from libc
                                    "int8_t" => "i8",
                                    "int16_t" => "i16",
                                    "int32_t" => "i32",
                                    "int64_t" => "i64",
                                    "uint8_t" => "u8",
                                    "uint16_t" => "u16",
                                    "uint32_t" => "u32",
                                    "uint64_t" => "u64",

                                    "c_schar" => "i8",
                                    "c_uchar" => "u8",
                                    "c_short" => "i16",
                                    "c_ushort" => "u16",
                                    "c_int" => "i32",
                                    "c_uint" => "u32",
                                    "c_float" => "f32",
                                    "c_double" => "f64",
                                    "c_longlong" => "i64",
                                    "c_ulonglong" => "u64",
                                    "intmax_t" => "i64",
                                    "uintmax_t" => "u64",

                                    "size_t" => "usize",
                                    "ptrdiff_t" => "isize",
                                    "intptr_t" => "isize",
                                    "uintptr_t" => "usize",
                                    "ssize_t" => "isize",

                                    #[cfg(all(not(target = "aarch64-unknown-linux-gnu"), not(target = "arm-unknown-linux-gnueabihf"), not(target = "arm-linux-androideabi")))]
                                    "c_char" => "i8",
                                    #[cfg(all(target = "aarch64-unknown-linux-gnu", target = "arm-unknown-linux-gnueabihf", target = "arm-linux-androideabi"))]
                                    "c_char" => "u8",
                                    _ => panic!("Unexpected C type")
                                };
                            writeln!(externs, "pub type {} = {};", name, typ).unwrap();
                        }
                    } else {
                        writeln!(externs, "pub type {} = *const ();", name).unwrap();
                    }
                }
                _ => ()
            }
        }

        println!("{}", include_str!("prelude.rs"));
        println!("{}", &gen_types.structs);
        println!("{}", &gen_types.enums);
        println!("{}", &gen_types.bitmasks);
        println!("{}", &gen_types.handles);
        println!("{}", &gen_types.typedefs);
        println!("{}", &gen_types.consts);
        println!("{}", &gen_types.externs);
        gen_types
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ConstType {
    /// A floating-point number
    Float,
    /// An unsigned integer
    Unsigned,
    ULong,
    ULongLong,
    USize,
    /// A string
    Str,
    Unknown
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
    fn features(&self)    -> &HashMap<VkVersion, VkFeature>;
    fn types(&self)       -> &HashMap<&str, VkType>;
    fn commands(&self)    -> &HashMap<&str, VkCommand>;
    fn extns(&self)       -> &HashMap<&str, VkExtn>;
    fn buffer_cap(&self)  -> usize;
    fn core_consts(&self) -> &Vec<&str>;
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