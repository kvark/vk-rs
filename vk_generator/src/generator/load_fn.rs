    fn_buf = load_fn({0}::RAW_NAME);
    if ptr::null() != fn_buf {{
        {0}::fn_ptr = mem::transmute(fn_buf); 
    }} else if {0}::fn_ptr == unloaded_function_panic as *const _ {{
        unloaded_fns.push({0}::RAW_NAME);
    }}
