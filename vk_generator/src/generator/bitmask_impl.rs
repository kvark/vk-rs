
impl {0} {{
    #[inline]
    pub fn empty() -> {0} {{
        {0} {{flags: 0}}
    }}

    #[inline]
    pub fn all() -> {0} {{
        {0} {{flags: 0b{1:b}}}
    }}

    #[inline]
    pub fn flags(self) -> {2} {{
        self.flags
    }}

    #[inline]
    pub fn from_flags(flags: {2}) -> Option<{0}> {{
        if flags & !0b{1:b} == 0 {{
            Some({0} {{flags: flags}})
        }} else {{
            None
        }}
    }}

    #[inline]
    pub fn from_flags_truncate(flags: {2}) -> {0} {{
        {0} {{flags: flags & 0b{1:b}}}
    }}

    #[inline]
    pub fn is_empty(self) -> bool {{
        self == {0}::empty()
    }}

    #[inline]
    pub fn is_all(self) -> bool {{
        self & {0}::all() == {0}::all()
    }}

    #[inline]
    pub fn intersects(self, other: {0}) -> bool {{
        self & other != {0}::empty()
    }}

    /// Returns true of `other` is a subset of `self`
    #[inline]
    pub fn subset(self, other: {0}) -> bool {{
        self & other == other
    }}
}}

impl BitOr for {0} {{
    type Output = {0};

    #[inline]
    fn bitor(self, rhs: {0}) -> {0} {{
        {0} {{flags: self.flags | rhs.flags }}
    }}
}}

impl BitOrAssign for {0} {{
    #[inline]
    fn bitor_assign(&mut self, rhs: {0}) {{
        *self = *self | rhs
    }}
}}

impl BitAnd for {0} {{
    type Output = {0};

    #[inline]
    fn bitand(self, rhs: {0}) -> {0} {{
        {0} {{flags: self.flags & rhs.flags}}
    }}
}}

impl BitAndAssign for {0} {{
    #[inline]
    fn bitand_assign(&mut self, rhs: {0}) {{
        *self = *self & rhs
    }}
}}

impl BitXor for {0} {{
    type Output = {0};

    #[inline]
    fn bitxor(self, rhs: {0}) -> {0} {{
        {0} {{flags: self.flags ^ rhs.flags}}
    }}
}}

impl BitXorAssign for {0} {{
    #[inline]
    fn bitxor_assign(&mut self, rhs: {0}) {{
        *self = *self ^ rhs
    }}
}}

impl Sub for {0} {{
    type Output = {0};

    #[inline]
    fn sub(self, rhs: {0}) -> {0} {{
        self & !rhs
    }}
}}

impl SubAssign for {0} {{
    #[inline]
    fn sub_assign(&mut self, rhs: {0}) {{
        *self = *self - rhs
    }}
}}

impl Not for {0} {{
    type Output = {0};

    #[inline]
    fn not(self) -> {0} {{
        self ^ {0}::all()
    }}
}}

