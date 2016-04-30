macro_rules! vk_struct_bindings {
    ($($raw_name: expr, $name: ident ($($param_name: ident: $param: ty),*,) -> $ret: ty);+;) => {
        $(type $name = unsafe extern "system" fn($($param),*) -> $ret);+;

        pub struct FnPtr {
            pub raw_name: &'static str,
            fn_ptr: *const ()
        }

        impl FnPtr {
            pub fn is_loaded(&self) -> bool {
                self.fn_ptr != unloaded_function_panic as *const ()
            }
        }

        pub struct Vk {
            $($name: FnPtr),+
        }

        impl Vk {
            pub fn new() -> Vk {unsafe{
                use std::mem;
                let mut vk: Vk = mem::uninitialized();

                $(
                    vk.$name = FnPtr{ raw_name: $raw_name, fn_ptr: unloaded_function_panic as *const ()};
                )+

                vk
            }}

            pub fn load_with<F: FnMut(&str) -> *const ()>(&mut self, mut load_fn: F) -> ::std::result::Result<(), Vec<&'static str>> {
                use std::ptr;
                let mut fn_buf: *const ();
                let mut unloaded_fns = Vec::new();

                $(
                    fn_buf = load_fn($raw_name);
                    if ptr::null() != fn_buf {
                        self.$name = FnPtr{ raw_name: $raw_name, fn_ptr: fn_buf };
                    } else if self.$name.fn_ptr != unloaded_function_panic as *const () {
                        unloaded_fns.push($raw_name)
                    }
                )+

                if 0 == unloaded_fns.len() {
                    Ok(())
                } else {
                    Err(unloaded_fns)
                }
            }

            $(
                pub unsafe extern "system" fn $name(&self, $($param_name: $param),*) -> $ret {
                    use std::mem;

                    mem::transmute::<_, $name>(self.$name.fn_ptr)($($param_name),*)
                }
            )+
        }
    }
}
