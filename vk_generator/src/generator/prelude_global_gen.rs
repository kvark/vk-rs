macro_rules! vk_functions {
    ($($raw_name: expr, $name: ident ($($param_name: ident: $param: ty),*,) -> $ret: ty;)+) => {
        $(
            pub unsafe extern "system" fn $name (
                $($param_name: $param),*
                ) -> $ret {
                use std::mem;

                mem::transmute::<_, $name::FnType>($name::fn_ptr)($($param_name),*)
            }

            pub mod $name {

                use super::super::*;
                #[allow(unused_imports)]
                use super::super::libc_reexports::*;
                pub const RAW_NAME: &'static str = $raw_name;
                pub static mut fn_ptr: *const () = unloaded_function_panic as *const ();
                #[doc(hidden)]
                pub type FnType = unsafe extern "system" fn($($param),*) -> $ret;

                pub fn is_loaded() -> bool {
                    unsafe{ fn_ptr == unloaded_function_panic as *const () }
                }
            }
        )+

        pub fn load_with<F: FnMut(&str) -> *const ()>(mut load_fn: F) -> ::std::result::Result<(), Vec<&'static str>> {unsafe{
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
