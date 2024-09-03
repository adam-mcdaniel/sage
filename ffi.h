void __test() {
    (++ffi_ptr)->i = 123456;
}

void __unsafe_memcpy() {
    cell *dst = ffi_ptr[-2].p,
         *src = ffi_ptr[-1].p,
         count = ffi_ptr[0];

    for (int64_t i=0; i<count.i; i++)
        dst[i] = src[i];    
}
