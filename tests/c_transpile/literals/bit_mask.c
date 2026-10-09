/* bit_mask.c: literal coverage for register flags and masks. */

static unsigned int pack_flags(void) {
    unsigned int f = 0u;
    f |= 0x01U;
    f |= 0x10ul;
    f |= 0007U;
    f |= 1u << 5;
    return f;
}

static int decode(const char *s) {
    char a = '\x42';
    char b = '\102';
    if (s[0] != 'f' || a != b) return -1;
    return (int)a;
}

static double gain(int step) {
    double g = 1.;
    g += step * 0.5;
    g += 1.5e-3;
    return g;
}

int bit_mask_check(void) {
    int fail = 0;

    unsigned int f = pack_flags();
    if (f != (0x01u | 0x10u | 7u | (1u << 5)) && fail == 0) fail = 1;

    unsigned long wide = 0xDEADBEEFUL;
    long low = 0xEFL;
    if ((wide & low) != 0xEFUL && fail == 0) fail = 2;
    if ((wide >> 8) == 0UL && fail == 0) fail = 3;

    char nl = '\n', tab = '\t', nul = '\0';
    char bs = '\\', sq = '\'', dq = '\"';
    if ((nl != 0x0A || tab != 0x09) && fail == 0) fail = 4;
    if (nul != 0 && fail == 0) fail = 5;
    if ((bs != 92 || sq != 39 || dq != 34) && fail == 0) fail = 6;

    const char *flags = "flag" "|" "mask";
    const char *path = "C:\\tmp\\dev\n";
    if (decode(flags) != 0x42 && fail == 0) fail = 7;
    if ((path[2] != '\\' || path[6] != '\\') && fail == 0) fail = 8;

    double g = gain(2);
    if ((g < 2.0 || g > 2.1) && fail == 0) fail = 9;

    unsigned int mask = 0xffu;
    int shift = 3;
    int probe = 0x1F;
    if ((int)((mask >> shift) & (unsigned)probe) != 0x1F && fail == 0) fail = 10;

    return fail;
}
