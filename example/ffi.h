void __memcpy() {
    union cell *dst = ffi_ptr[-2].p, *src = ffi_ptr[-1].p, n = ffi_ptr[0];
    ffi_ptr -= 3;
    memcpy(dst, src, n.i * sizeof(union cell));
}