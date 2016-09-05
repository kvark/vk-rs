impl Clone for {0} {{
    fn clone(&self) -> {0} {{
        use std::ptr;
        use std::mem;
        unsafe {{
            let mut cloned = mem::zeroed();
            ptr::copy_nonoverlapping(self, &mut cloned, 1);

            cloned
        }}
    }}
}}
