void __unsafe_memcpy() {
    union cell *dst = ffi_ptr[-2].p, *src = ffi_ptr[-1].p;
    long long int n = ffi_ptr[0].i;
	ffi_ptr -= 3;
    memcpy(dst, src, n * sizeof(union cell));
}