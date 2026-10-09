/* unit_scale.c: literal coverage for unit conversion constants. */
static const double tiny_ratio = 1.0e-6;
static const float big_ratio = 1.0e6F;

static int scaled_units(int meters) {
    int base = 100, fine = 010, bump = 0x10;
    int mask = 0x1F & 0x0F;
    return meters * base + fine + bump - mask;
}

static double blend(int parts, int whole) {
    if (whole == 0) return .5;
    double r = (double)parts / (double)whole;
    return r * 1.;
}

static int tag_start(const char *s) {
    return s[0] == 'u' && s[1] == 'n';
}

int unit_scale_check(void) {
    int fail = 0;
    if (scaled_units(3) != 3 * 100 + 8 + 16 - 15) fail = 1;

    unsigned int flags = 0u;
    flags |= 0x1U; flags |= 0x2ul; flags |= 010L;
    if (flags != (1u | 2u | 8u) && fail == 0) fail = 2;

    long span = 2500000L;
    unsigned long cap = 4000000000UL;
    if (span / 1000L != 2500L && fail == 0) fail = 3;
    if (cap != 4000000000UL && fail == 0) fail = 4;

    if (blend(1, 4) != 0.25 && fail == 0) fail = 5;
    if (tiny_ratio > 1.0 && fail == 0) fail = 6;
    if (big_ratio != 1.0e6F && fail == 0) fail = 7;

    char nl = '\n', tab = '\t', nul = '\0';
    char bs = '\\', sq = '\'', hx = '\x41', oc = '\101';
    if ((nl != 10 || tab != 9 || nul != 0) && fail == 0) fail = 8;
    if ((bs != 92 || sq != 39) && fail == 0) fail = 9;
    if ((hx != oc || hx != 65) && fail == 0) fail = 10;

    const char *msg = "unit" "scale" "\tready\n";
    const char *esc = "A=\x41" " B=\101" " q=\"" " b=\\";
    if (!tag_start(msg) && fail == 0) fail = 11;
    if ((esc[2] != 'A' || esc[6] != 'A') && fail == 0) fail = 12;
    if ((esc[10] != '\"' || esc[14] != '\\') && fail == 0) fail = 13;
    return fail;
}
