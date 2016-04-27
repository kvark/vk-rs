use self::types::*;
pub use std::os::raw::c_ulonglong;

#[doc(hidden)]
pub fn unloaded_function_panic() -> ! {
    panic!("Attempted to run unloaded vulkan function")
}

macro_rules! vk_functions {
    ($($raw_name: expr, $name: ident ($($param_name: ident: $param: ty),*,) -> $ret: ty);+;) => {
        $(
            pub unsafe extern "system" fn $name (
                $($param_name: $param),*
                ) -> $ret {
                use std::mem;

                mem::transmute::<_, $name::FnType>($name::fn_ptr)($($param_name),*)
            }

            pub mod $name {
                use super::*;
                use super::types::*;
                pub const RAW_NAME: &'static str = $raw_name;
                pub static mut fn_ptr: *const () = unloaded_function_panic as *const ();
                #[doc(hidden)]
                pub type FnType = unsafe extern "system" fn($($param),*) -> $ret;

                pub fn is_loaded() -> bool {
                    unsafe{ fn_ptr == unloaded_function_panic as *const () }
                }
            }
        )+

        pub fn load_with<F: FnMut(&str) -> *const ()>(mut load_fn: F) -> Result<(), Vec<&'static str>> {unsafe{
            use std::ptr; 
            let mut fn_buf: *const (); 
            let mut unloaded_fns = Vec::new();

            $(
                fn_buf = load_fn($raw_name);
                if ptr::null() != fn_buf {
                    $name::fn_ptr = fn_buf;
                } else if $name::fn_ptr != unloaded_function_panic as *const () {
                    unloaded_fns.push($raw_name)
                }
            )+

            if 0 == unloaded_fns.len() {
                Ok(())
            } else {
                Err(unloaded_fns)
            }
        }}
    }
}

macro_rules! handle_nondispatchable {
    ($name: ident) => {
        #[repr(C)]
        #[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
        pub struct $name (uint64_t);

        impl fmt::Pointer for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
                write!(f, "0x{:x}", self.0)
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
                write!(f, "0x{:x}", self.0)
            }
        }
    }
}

macro_rules! vk_bitflags_wrapped {
    ($name: ident, $all: expr, $flag_type: ty) => {
        #[repr(C)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {flags: $flag_type}

        impl $name {
            #[inline]
            pub fn empty() -> $name {
                $name {flags: 0}
            }

            #[inline]
            pub fn all() -> $name {
                $name {flags: $all}
            }

            #[inline]
            pub fn flags(self) -> $flag_type {
                self.flags
            }

            #[inline]
            pub fn from_flags(flags: $flag_type) -> Option<$name> {
                if flags & !$all == 0 {
                    Some($name {flags: flags})
                } else {
                    None
                }
            }

            #[inline]
            pub fn from_flags_truncate(flags: $flag_type) -> $name {
                $name {flags: flags & $all}
            }

            #[inline]
            pub fn is_empty(self) -> bool {
                self == $name::empty()
            }

            #[inline]
            pub fn is_all(self) -> bool {
                self & $name::all() == $name::all()
            }

            #[inline]
            pub fn intersects(self, other: $name) -> bool {
                self & other != $name::empty()
            }

            /// Returns true of `other` is a subset of `self`
            #[inline]
            pub fn subset(self, other: $name) -> bool {
                self & other == other
            }
        }

        impl BitOr for $name {
            type Output = $name;

            #[inline]
            fn bitor(self, rhs: $name) -> $name {
                $name {flags: self.flags | rhs.flags }
            }
        }

        impl BitOrAssign for $name {
            #[inline]
            fn bitor_assign(&mut self, rhs: $name) {
                *self = *self | rhs
            }
        }

        impl BitAnd for $name {
            type Output = $name;

            #[inline]
            fn bitand(self, rhs: $name) -> $name {
                $name {flags: self.flags & rhs.flags}
            }
        }

        impl BitAndAssign for $name {
            #[inline]
            fn bitand_assign(&mut self, rhs: $name) {
                *self = *self & rhs
            }
        }

        impl BitXor for $name {
            type Output = $name;

            #[inline]
            fn bitxor(self, rhs: $name) -> $name {
                $name {flags: self.flags ^ rhs.flags}
            }
        }

        impl BitXorAssign for $name {
            #[inline]
            fn bitxor_assign(&mut self, rhs: $name) {
                *self = *self ^ rhs
            }
        }

        impl Sub for $name {
            type Output = $name;

            #[inline]
            fn sub(self, rhs: $name) -> $name {
                self & !rhs
            }
        }

        impl SubAssign for $name {
            #[inline]
            fn sub_assign(&mut self, rhs: $name) {
                *self = *self - rhs
            }
        }

        impl Not for $name {
            type Output = $name;

            #[inline]
            fn not(self) -> $name {
                self ^ $name::all()
            }
        }
    }
}
