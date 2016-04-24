mod crawler;

use to_option;
use generator::GenRegistry;
use xml::{EventReader, ParserConfig};

use std::{fmt, mem};
use std::collections::HashMap;

#[inline]
fn null_str() -> *const str {
    unsafe{ mem::transmute([0usize; 2] ) }
}

pub struct VkRegistry<'a> {
    string_buffer: String,
    types: HashMap<&'a str, VkType>,
    core_consts: Vec<&'a str>,
    commands: HashMap<&'a str, VkCommand>,
    features: HashMap<VkVersion, VkFeature>,
    extns: HashMap<&'a str, VkExtn>
}

impl<'a> VkRegistry<'a> {
    pub fn new(vk_xml: &[u8]) -> VkRegistry<'a> {
        let mut registry = VkRegistry {
            string_buffer: String::with_capacity(vk_xml.len()),
            types: HashMap::with_capacity(512),
            core_consts: Vec::with_capacity(16),
            commands: HashMap::with_capacity(256),
            features: HashMap::with_capacity(8),
            extns: HashMap::with_capacity(64)
        };
        let xml_reader = EventReader::new_with_config(vk_xml, ParserConfig::new().trim_whitespace(true));
        crawler::crawl(xml_reader.into_iter(), &mut registry);
        registry
    }

    fn push_type(&mut self, vk_type: VkType) -> Result<(), ()> {
        if let VkType::ApiConst{name, ..} = vk_type {
            self.core_consts.push(unsafe{ &*name });
        }

        match vk_type {
            VkType::Unhandled     => Err(()),
            vk_type               => unsafe{ 
                let name = vk_type.name().unwrap();
                if "API Constants" != &*name {
                    self.types.insert(&*vk_type.name().unwrap(), vk_type); 
                    Ok(())
                } else {Err(())}
            }
        }
    }

    fn push_command(&mut self, vk_command: Option<VkCommand>) -> Result<(), ()> {
        if let Some(cmd) = vk_command {
            unsafe{ self.commands.insert(&*cmd.name, cmd) };
            Ok(())
        } else {Err(())}
    }

    fn push_feature(&mut self, feature: Option<VkFeature>) -> Result<(), ()> {
        if let Some(feat) = feature {
            self.features.insert(feat.version, feat);
            Ok(())
        } else {Err(())}
    }

    fn push_extn(&mut self, extn: Option<VkExtn>) -> Result<(), ()> {
        if let Some(ex) = extn {
            unsafe{ self.extns.insert(&*ex.name, ex) };
            Ok(())
        } else {Err(())}
    }

    /// Append a given attribute to the internal string buffer and return an unsafe slice into the buffer string
    fn append_str(&mut self, string: &str) -> *const str {
        use std::{slice, str};

        let prepushcap = self.string_buffer.capacity();
        // We want to have all of the string in one block of memory in order to save heap allocation time. 
        self.string_buffer.push_str(string);

        if prepushcap != self.string_buffer.capacity() {
            panic!("Allocation detected in string buffer")
        }

        unsafe {
            let ptr = self.string_buffer.as_ptr().offset((self.string_buffer.len() - string.len()) as isize);
            str::from_utf8_unchecked(slice::from_raw_parts(ptr, string.len())) as *const str
        }
    }
}

impl<'a> GenRegistry for VkRegistry<'a> {
    fn features(&self) -> &HashMap<VkVersion, VkFeature> {
        &self.features
    }

    fn types(&self) -> &HashMap<&str, VkType> {
        &self.types
    }

    fn commands(&self) -> &HashMap<&str, VkCommand> {
        &self.commands
    }

    fn extns(&self) -> &HashMap<&str, VkExtn> {
        &self.extns
    }

    fn buffer_cap(&self) -> usize {
        self.string_buffer.capacity()
    }

    fn core_consts(&self) -> &Vec<&str> {
        &self.core_consts
    }
}

/// The type of a Vulkan element (struct members, union variants, function parameters, etc.) and
/// any associated data.
#[derive(Clone)]
pub enum VkElType {
    /// A standard, singular, owned field
    Var(*const str),
    /// An intermediate type created when the keyword "const" is detected
    Const(*const str),
    ConstPtr(*const str, u8),
    MutPtr(*const str, u8),
    /// An array whose contents are immutable
    ConstArray(*const str, usize),
    /// An array whose contents are mutable
    MutArray(*const str, usize),
    /// An const array that uses an API constant as the size
    ConstArrayEnum(*const str, *const str),
    /// A mutable array that uses an API constant as the size
    MutArrayEnum(*const str, *const str),
    /// Nothing. Equivilant to () in Rust
    Void,
    /// Default value to initialize with.
    Unknown
}

impl VkElType {
    pub fn type_ptr(&self) -> Option<*const str> {
        use self::VkElType::*;
        match *self {
            Var(s)               |
            Const(s)             |
            ConstPtr(s, _)       |
            MutPtr(s, _)         |
            ConstArray(s, _)     |
            ConstArrayEnum(s, _) |
            MutArray(s, _)       |
            MutArrayEnum(s, _)  => Some(s),
            Void                 |
            Unknown             => None
        }
    }

    pub fn set_type(&mut self, typ: *const str) {
        use self::VkElType::*;
        match *self {
            Var(ref mut s)               |
            Const(ref mut s)             |
            ConstPtr(ref mut s, _)       |
            MutPtr(ref mut s, _)         | 
            ConstArray(ref mut s, _)     |
            ConstArrayEnum(ref mut s, _) |
            MutArray(ref mut s, _)       |
            MutArrayEnum(ref mut s, _)  => *s = typ,
            Void                        => panic!("Field type already set"),
            Unknown                     => *self = Var(typ)

        }
    }

    fn make_const(&mut self) {
        use self::VkElType::*;
        match *self {
            Var(s)               |
            Const(s)            => *self = Const(s),
            ConstPtr(_, _)       |
            MutPtr(_, _)        => panic!("Attempted changing mutability of pointer"),
            ConstArray(_, _)     |
            ConstArrayEnum(_, _) |
            MutArray(_, _)       |
            MutArrayEnum(_, _)  => panic!("Attempted changing mutability of array"),
            Void                 |
            Unknown             => *self = Const(null_str())
        }
    }

    fn make_ptr(&mut self, count: u8) {
        use self::VkElType::*;
        match *self {
            Var(s)              => *self = MutPtr(s, count),
            Const(s)            => *self = ConstPtr(s, count),
            MutPtr(_, _)         |
            ConstPtr(_, _)      => panic!("Attempted to change type from pointer to pointer"),
            ConstArray(_, _)     |
            ConstArrayEnum(_, _) |
            MutArrayEnum(_, _)   |
            MutArray(_, _)      => panic!("Attempted to change type from array to pointer"),
            Void                 |
            Unknown             => *self = MutPtr(null_str(), count)
        }
    }

    fn make_array(&mut self, size: usize) {
        use self::VkElType::*;
        match *self {
            Var(s)              => *self = if size == 0 {MutArrayEnum(s, null_str())} else {MutArray(s, size)},
            Const(s)            => *self = if size == 0 {ConstArrayEnum(s, null_str())} else {ConstArray(s, size)},
            MutPtr(_, _)         |
            ConstPtr(_, _)      => panic!("Attempted to change type from pointer to array"),
            ConstArray(_, _)     |
            ConstArrayEnum(_, _) |
            MutArrayEnum(_, _)   |
            MutArray(_, _)      => panic!("Attempted to change type from array to array"),
            Void                 |
            Unknown             => panic!("Attempted to change type to array without type identifier")
        }
    }

    fn set_array_const(&mut self, size_enum: *const str) {
        use self::VkElType::*;
        match *self {
            Var(_)                      => panic!("Attempted to set array length of Var"),
            Const(_)                    => panic!("Attempted to set array length of Const"),
            MutPtr(_, _)                 |
            ConstPtr(_, _)              => panic!("Attempted to set array length of Ptr"),
            ConstArrayEnum(_, ref mut e) |
            MutArrayEnum(_, ref mut e)  => *e = size_enum,
            ConstArray(_, _)             |
            MutArray(_, _)              => panic!("Attempted to change array length of array with known size"),
            Void                         |
            Unknown                     => panic!("Attempted to set array length of unknown/void type")
        }
    }

    fn make_void(&mut self) {
        use self::VkElType::*;
        match *self {
            Var(_)              => panic!("Attempted to make Var a void type"),
            Const(_)            => panic!("Attempted to make Const a void type"),
            MutPtr(_, _)         |
            ConstPtr(_, _)      => panic!("Attempted to make Ptr a void type"),
            ConstArrayEnum(_, _) |
            MutArrayEnum(_, _)   |
            ConstArray(_, _)     |
            MutArray(_, _)      => panic!("Attempted to change array length of array with known size"),
            Void                => panic!("Attempted redundant setting of Void to Void"),
            Unknown             => *self = Void
        }
    }
}

impl fmt::Debug for VkElType {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::VkElType::*;
        if let Unknown = *self {
            write!(fmt, "Unknown")
        } else if let Void = *self {
            write!(fmt, "Void")
        } else {
            let pt;
            let mut fmt_tuple = fmt.debug_tuple(
                match *self {
                    Var(p)               => {pt = p; "Var"}
                    Const(p)             => {pt = p; "Const"}
                    ConstPtr(p, _)       => {pt = p; "ConstPtr"}
                    MutPtr(p, _)         => {pt = p; "MutPtr"}
                    ConstArray(p, _)     => {pt = p; "ConstArray"}
                    ConstArrayEnum(p, _) => {pt = p; "ConstArrayEnum"}
                    MutArray(p, _)       => {pt = p; "MutArray"}
                    MutArrayEnum(p,_)    => {pt = p; "MutArrayEnum"}
                    Void                  |
                    Unknown              => unreachable!()
                });
            fmt_tuple.field(&to_option(pt));
            match *self {
                ConstPtr(_, c)       |
                MutPtr(_, c)        => fmt_tuple.field(&c),
                ConstArray(_, s)     |
                MutArray(_, s)      => fmt_tuple.field(&s),
                ConstArrayEnum(_, e) |
                MutArrayEnum(_, e)  => fmt_tuple.field(&to_option(e)),
                _                   => &mut fmt_tuple
            }.finish()
        }
    }
}

#[derive(Clone)]
pub struct VkMember {
    pub field_type: VkElType,
    pub field_name: *const str,
    pub optional: bool
}

impl fmt::Debug for VkMember {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt .debug_struct("VkMember")
            .field("type", &self.field_type)
            .field("name", &to_option(self.field_name))
            .field("optional", &self.optional)
            .finish()
    }
}

impl VkMember {
    fn empty(optional: bool) -> VkMember {
        VkMember {
            field_type: VkElType::Unknown,
            field_name: null_str(),
            optional: optional
        }
    }

    fn set_name(&mut self, field_name: *const str) {
        if self.field_name != null_str() {
            panic!("Unexpected \"name\" tag");
        } else {
            self.field_name = field_name;
        }
    }
}

/// A variant of a vulkan enum
#[derive(Clone)]
pub enum VkVariant {
    Value {
        name: *const str,
        value: isize
    },

    Bitpos {
        name: *const str,
        bitpos: u32
    }
}

impl fmt::Debug for VkVariant {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::VkVariant::*;

        let pt;
        let mut fmt_struct = fmt.debug_struct(
            match *self {
                Value{name, ..}    => {pt = name; "Value"}
                Bitpos{name, ..}   => {pt = name; "Bitpos"}
            });
        fmt_struct.field("name", &to_option(pt));
        match *self {
            Value{value, ..} => fmt_struct.field("value", &value),
            Bitpos{bitpos, ..} => fmt_struct.field("bitpos", &bitpos),
        }.finish()
    }
}

impl VkVariant {
    fn new_value(name: *const str, value: isize) -> VkVariant {
        VkVariant::Value {
            name: name,
            value: value
        }
    }

    fn new_bitpos(name: *const str, bitpos: u32) -> VkVariant {
        VkVariant::Bitpos {
            name: name,
            bitpos: bitpos
        }
    }

    pub fn name(&self) -> *const str {
        use self::VkVariant::*;
        match *self {
            Value{name, ..} |
            Bitpos{name, ..} => name
        }
    }

    pub fn set_name(&mut self, new_name: *const str) {
        use self::VkVariant::*;
        match *self {
            Value{ref mut name, ..} |
            Bitpos{ref mut name, ..} => *name = new_name
        }
    }
}

#[derive(Clone)]
pub enum VkType {
    Struct {
        name: *const str,
        fields: Vec<VkMember>
    },

    Union {
        name: *const str,
        variants: Vec<VkMember>
    },

    Enum {
        name: *const str,
        variants: Vec<VkVariant>
    },

    Bitmask {
        name: *const str,
        variants: Vec<VkVariant>
    },

    Handle {
        name: *const str,
        validity: bool,
        dispatchable: bool
    },

    TypeDef {
        /// The type that is being aliased
        typ: *const str,
        /// The name of the new type
        name: *const str,
        /// Optional
        requires: *const str,
        validity: u8
    },

    ApiConst {
        name: *const str,
        value: *const str
    },

    /// Defines are hardcoded into the generator, as procedurally generating them would be hard as fuck
    Define {
        name: *const str
    },
    /// Same goes for function pointers
    FuncPointer {
        name: *const str
    },

    ExternType {
        name: *const str,
        requires: *const str
    },

    Unhandled
}

impl VkType {
    pub fn name(&self) -> Option<*const str> {
        use self::VkType::*;
        match *self {
            Struct{name, ..}       |
            Union{name, ..}        |
            Enum{name, ..}         |
            Bitmask{name, ..}      |
            Handle{name, ..}       |
            TypeDef{name, ..}      |
            ApiConst{name, ..}     |
            Define{name, ..}       |
            FuncPointer{name, ..}  |
            ExternType{name, ..}  => Some(name),
            Unhandled             => None
        }
    }

    pub fn set_name(&mut self, new_name: *const str) -> Result<(), ()> {
        use self::VkType::*;
        match *self {
            Struct{ref mut name, ..}       |
            Union{ref mut name, ..}        |
            Enum{ref mut name, ..}         |
            Bitmask{ref mut name, ..}      |
            Handle{ref mut name, ..}       |
            TypeDef{ref mut name, ..}      |
            ApiConst{ref mut name, ..}     |
            Define{ref mut name, ..}       |
            FuncPointer{ref mut name, ..}  |
            ExternType{ref mut name, ..}  => {*name = new_name; Ok(())}
            Unhandled                     => Err(())
        }
    }

    pub fn new_struct(name: *const str) -> VkType {
        VkType::Struct {
            name: name,
            fields: Vec::with_capacity(8)
        }
    }

    pub fn new_union(name: *const str) -> VkType {
        VkType::Union {
            name: name,
            variants: Vec::with_capacity(8)
        }
    }

    pub fn new_enum(name: *const str) -> VkType {
        VkType::Enum {
            name: name,
            variants: Vec::with_capacity(8)
        }
    }

    pub fn new_bitmask(name: *const str) -> VkType {
        VkType::Bitmask {
            name: name,
            variants: Vec::with_capacity(8)
        }
    }

    pub fn empty_handle() -> VkType {
        VkType::Handle {
            name: null_str(),
            validity: false,
            dispatchable: true
        }
    }

    pub fn new_typedef(requires: Option<*const str>) -> VkType {
        use self::tdvalid::*;
        VkType::TypeDef {
            typ: null_str(),
            name: null_str(),
            requires: requires.unwrap_or(null_str()),
            validity: NOSEMICOLON | NOTYPEDEF
        }
    }

    pub fn new_const(name: *const str, value: *const str) -> VkType {
        VkType::ApiConst {
            name: name,
            value: value
        }
    }

    pub fn new_define(name: *const str) -> VkType {
        VkType::Define  {
            name: name
        }
    }

    pub fn empty_define() -> VkType {
        VkType::Define {
            name: null_str()
        }
    }

    pub fn empty_funcpointer() -> VkType {
        VkType::FuncPointer {
            name: null_str()
        }
    }

    pub fn new_extern(name: *const str, requires: *const str) -> VkType {
        VkType::ExternType {
            name: name,
            requires: requires
        }
    }
}

/// VkType::TypeDef validity flags
pub mod tdvalid {
    pub const NOSEMICOLON: u8 = 0b01;
    pub const NOTYPEDEF: u8   = 0b10;
}

#[derive(Clone)]
pub struct VkCommand {
    /// The return value
    pub ret: VkElType,
    pub name: *const str,
    pub params: Vec<VkParam>
}

impl fmt::Debug for VkCommand {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt .debug_struct("VkCommand")
            .field("ret", &self.ret)
            .field("name", &to_option(self.name))
            .field("params", &self.params)
            .finish()
    }
}

impl VkCommand {
    fn empty() -> VkCommand {
        VkCommand {
            ret: VkElType::Unknown,
            name: null_str(),
            params: Vec::with_capacity(8)
        }
    }
}

#[derive(Clone)]
pub struct VkParam {
    pub typ: VkElType,
    pub name: *const str
}

impl fmt::Debug for VkParam {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt .debug_struct("VkParam")
            .field("typ", &self.typ)
            .field("name", &to_option(self.name))
            .finish()
    }
}

impl VkParam {
    fn empty() -> VkParam {
        VkParam {
            typ: VkElType::Unknown,
            name: null_str()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VkVersion(pub u16, pub u16);

impl VkVersion {
    fn from_str(num: &str) -> VkVersion {
        use std::u16;

        let mut ver = [0; 2];
        for (i, digit) in num.split('.').enumerate() {
            ver[i] = u16::from_str_radix(digit, 10).unwrap();
        }

        VkVersion(ver[0], ver[1])
    }
}

pub struct VkFeature {
    pub name: *const str,
    pub version: VkVersion,
    pub require: Vec<VkInterface>,
    pub remove: Vec<VkInterface>
}

impl VkFeature {
    fn new(name: *const str, version: VkVersion) -> VkFeature {
        VkFeature {
            name: name,
            version: version,
            require: Vec::with_capacity(16),
            remove: Vec::with_capacity(16)
        }
    }

    fn push_command(&mut self, name: *const str, reqrem: &VkReqRem) {
        use self::VkReqRem::*;

        match *reqrem {
            Require(profile) => self.require.push(VkInterface::new_command(name, profile)),
            Remove(profile)  => self.remove.push(VkInterface::new_command(name, profile)),
            None             => panic!("Invalid reqrem")
        }
    }

    fn push_const(&mut self, name: *const str, reqrem: &VkReqRem) {
        use self::VkReqRem::*;

        match *reqrem {
            Require(profile) => self.require.push(VkInterface::new_api_const(name, profile)),
            Remove(profile)  => self.remove.push(VkInterface::new_api_const(name, profile)),
            None             => panic!("Invalid reqrem")
        }
    }

    fn push_type(&mut self, name: *const str, reqrem: &VkReqRem) {
        use self::VkReqRem::*;

        match *reqrem {
            Require(profile) => self.require.push(VkInterface::new_type(name, profile)),
            Remove(profile)  => self.remove.push(VkInterface::new_type(name, profile)),
            None             => panic!("Invalid reqrem")
        }
    }
}

pub enum VkInterface {
    Command {
        name: *const str,
        profile: *const str
    },

    Type {
        name: *const str,
        profile: *const str
    },

    /// A new constant that is defined in an extension.
    ConstDef {
        name: *const str,
        value: *const str,
        profile: *const str
    },

    /// Defines interface to constant defined in "API Constants" enum tag
    ApiConst {
        name: *const str,
        profile: *const str
    },

    ExtnEnum {
        extends: *const str,
        profile: *const str,
        variant: VkVariant
    }
}

impl fmt::Debug for VkInterface {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::VkInterface::*;

        let mut fmt_struct = fmt.debug_struct(
            match *self {
                Command{..}  => "Command",
                Type{..}     => "Type",
                ConstDef{..} => "ConstDef",
                ApiConst{..}  => "ApiConst",
                ExtnEnum{..} => "ExtnEnum"
            });
        match *self {
            Command{name, profile}  |
            Type{name, profile}     |
            ApiConst{name, profile} =>
                fmt_struct.field("name", &to_option(name))
                          .field("profile", &to_option(profile))
                          .finish(),
            ExtnEnum{extends,
                     profile, 
                     ref variant}  =>
                fmt_struct.field("extends", &to_option(extends))
                          .field("profile", &to_option(profile))
                          .field("variant", &*variant)
                          .finish(),
            ConstDef{name, 
                     value, 
                     profile}      =>
                fmt_struct.field("name", &to_option(name))
                          .field("value", &to_option(value))
                          .field("profile", &to_option(profile))
                          .finish()

        }
    }
}

impl VkInterface {
    fn new_command(name: *const str, profile: Option<*const str>) -> VkInterface {
        VkInterface::Command {
            name: name,
            profile: profile.unwrap_or(null_str())
        }
    }

    fn new_type(name: *const str, profile: Option<*const str>) -> VkInterface {
        VkInterface::Type {
            name: name,
            profile: profile.unwrap_or(null_str())
        }
    }

    fn new_const_def(name: *const str, value: *const str, profile: Option<*const str>) -> VkInterface {
        VkInterface::ConstDef {
            name: name,
            value: value,
            profile: profile.unwrap_or(null_str())
        }
    }

    fn new_api_const(name: *const str, profile: Option<*const str>) -> VkInterface {
        VkInterface::ApiConst {
            name: name,
            profile: profile.unwrap_or(null_str())
        }
    }

    fn new_extn_enum(variant: VkVariant, extends: Option<*const str>, profile: Option<*const str>) -> VkInterface {
        VkInterface::ExtnEnum {
            extends: extends.unwrap_or(null_str()),
            profile: profile.unwrap_or(null_str()),
            variant: variant
        }
    }
}

/// When loading a VkFeature, this stores whether or not we're in a require or remove block.
enum VkReqRem {
    Require(Option<*const str>),
    Remove(Option<*const str>),
    None
}

pub struct VkExtn {
    pub name: *const str,
    pub num: isize,
    pub require: Vec<VkInterface>,
    pub remove: Vec<VkInterface>
}

impl VkExtn {
    fn new(name: *const str, num: isize) -> VkExtn {
        VkExtn {
            name: name,
            num: num,
            require: Vec::with_capacity(8),
            // Most, if not all, extensions don't have remove tags so this is just here for contingency
            remove: Vec::new()
        }
    }

    fn push_command(&mut self, name: *const str, reqrem: &VkReqRem) {
        use self::VkReqRem::*;

        match *reqrem {
            Require(profile) => self.require.push(VkInterface::new_command(name, profile)),
            Remove(profile)  => self.remove.push(VkInterface::new_command(name, profile)),
            None             => panic!("Invalid reqrem")
        }
    }

    fn push_const(&mut self, name: *const str, value: *const str, reqrem: &VkReqRem) {
        use self::VkReqRem::*;

        match *reqrem {
            Require(profile) => self.require.push(VkInterface::new_const_def(name, value, profile)),
            Remove(profile)  => self.remove.push(VkInterface::new_const_def(name, value, profile)),
            None             => panic!("Invalid reqrem")
        }
    }

    fn push_enum(&mut self, variant: VkVariant, extends: Option<*const str>, reqrem: &VkReqRem) {
        use self::VkReqRem::*;

        match *reqrem {
            Require(profile) => self.require.push(VkInterface::new_extn_enum(variant, extends, profile)),
            Remove(profile)  => self.remove.push(VkInterface::new_extn_enum(variant, extends, profile)),
            None             => panic!("Invalid reqrem")
        }
    }

    fn push_type(&mut self, name: *const str, reqrem: &VkReqRem) {
        use self::VkReqRem::*;

        match *reqrem {
            Require(profile) => self.require.push(VkInterface::new_type(name, profile)),
            Remove(profile)  => self.remove.push(VkInterface::new_type(name, profile)),
            None             => panic!("Invalid reqrem")
        }
    }
}