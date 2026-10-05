void *memset(void *s, int c, unsigned long n);
void *memcpy(void *d, const void *s, unsigned long n);
void *memmove(void *d, const void *s, unsigned long n);

int builtin_memops(int n) {
    unsigned char buf[16];
    memset(buf, 0, 16);
    buf[0] = 5;
    memcpy(buf, buf, n);
    memmove(buf, buf, sizeof(buf));
    return buf[0];
}
