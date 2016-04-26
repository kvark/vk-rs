{0}: {{
    use std::mem;
    let mut array: [_; {1}] = unsafe{{ mem::uninitialized() }};

    for (i, e) in array[..].iter_mut().enumerate() {{
        *e = self.{0}[i].clone();
    }}
    array
}},