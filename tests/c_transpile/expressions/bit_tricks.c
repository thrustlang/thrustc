static unsigned int rotl(unsigned int v, unsigned int n) {
    return (v << n) | (v >> (32u - n));
}

static unsigned int popcount(unsigned int v) {
    unsigned int c = 0;
    while (v != 0u) {
        c += v & 1u;
        v >>= 1;
    }
    return c;
}

static int parity(int v) {
    unsigned int u = (unsigned int)v;
    u ^= u >> 16;
    u ^= u >> 8;
    u ^= u >> 4;
    u ^= u >> 2;
    u ^= u >> 1;
    return (int)(u & 1u);
}

static unsigned int gray(unsigned int v) {
    return v ^ (v >> 1);
}

int bit_tricks_check(void) {
    unsigned int or_all = 0xf0u | 0x0fu;
    unsigned int and_mask = 0xffu & 0x3cu;
    unsigned int xor_all = 0xa5u ^ 0x5au;
    unsigned int not_all = ~0u;
    unsigned int not_byte = ~0xffu;
    unsigned int shl = 1u << 5;
    unsigned int shr = 0x8000u >> 4;
    unsigned int rot = rotl(0x80000001u, 1u);
    unsigned int ones = popcount(0xf0f0u);
    int p_even = parity(3);
    int p_odd = parity(7);
    unsigned int g1 = gray(7u);
    unsigned int g2 = gray(4u);
    unsigned int chained = (and_mask ^ or_all) | (shl & shr);

    if (or_all != 0xffu) return 1;
    if (and_mask != 0x3cu) return 2;
    if (xor_all != 0xffu) return 3;
    if (not_all != 0xffffffffu) return 4;
    if ((not_byte & 0xffu) != 0u) return 5;
    if (shl != 32u) return 6;
    if (shr != 0x800u) return 7;
    if (rot != 3u) return 8;
    if (ones != 8u) return 9;
    if (p_even != 0) return 10;
    if (p_odd != 1) return 12;
    if (g1 != 4u) return 13;
    if (g2 != 6u) return 14;
    if (chained != ((0x3cu ^ 0xffu) | (32u & 0x800u))) return 15;

    return 0;
}
