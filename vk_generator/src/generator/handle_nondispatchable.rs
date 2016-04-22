#[repr(C)]
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub struct {0} (uint64_t);

impl fmt::Pointer for {0} {{
    fn fmt(&self, &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {{
        write!(f, "0x{{:x}}", self.0)
    }}
}}

impl fmt::Debug for {0} {{
    fn fmt(&self, &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {{
        write!(f, "0x{{:x}}", self.0)
    }}
}}

