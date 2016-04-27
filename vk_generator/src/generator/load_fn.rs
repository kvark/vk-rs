    fn_buf = load_fn({0}::RAW_NAME);
    if ptr::null() != fn_buf {{
        {0}::fn_ptr = mem::transmute(fn_buf); 
    }} else {{
        unloaded_fns.push({0}::RAW_NAME);
    }}
    