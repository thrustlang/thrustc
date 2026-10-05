/*
 * The translated sizes below are ABI literals for Linux x86_64:
 * struct { int; char; } pads to 8 bytes.
 */
unsigned long size_anon(void) {
    return sizeof(struct { int a; char b; });
}
