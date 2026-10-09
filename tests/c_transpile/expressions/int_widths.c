#include <stdint.h>

static int narrow_roundtrip(int value) {
    signed char sc = (signed char)value;
    unsigned char uc = (unsigned char)value;
    short sh = (short)value;
    unsigned short us = (unsigned short)value;
    return (int)sc + (int)uc + (int)sh + (int)us;
}

static unsigned int pack_nibbles(unsigned int hi, unsigned int lo) {
    return ((hi & 0xfu) << 4) | (lo & 0xfu);
}

static int sign_extend(unsigned char byte) {
    return (int)(signed char)byte;
}

static uint32_t fold_words(uint32_t x, uint32_t y) {
    uint64_t wide = (uint64_t)x * (uint64_t)y;
    return (uint32_t)(wide ^ (wide >> 32));
}

int int_widths_check(void) {
    int big = 0x12345678;
    unsigned int ubig = 0xfedcba98u;
    unsigned char uc = (unsigned char)big;
    short sh = (short)big;
    unsigned short ush = (unsigned short)big;
    long lg = (long)big;
    int back = (int)lg;
    unsigned long ulg = (unsigned long)ubig;
    int round, pack, tot;
    unsigned int wrap;

    uc = uc + 0;
    sh = sh + 0;
    ush = ush + 0;
    back = back + 0;
    ulg = ulg + 0;
    round = narrow_roundtrip(big);
    wrap = (unsigned int)-1 & 0xffu;
    pack = (int)pack_nibbles(0xau, 0x5u);
    tot = (int)fold_words(0x12345678u, 0x9abcdef0u);

    if (uc != 0x78u) return 1;
    if (sh != 0x5678) return 2;
    if (ush != 0x5678u) return 3;
    if (back != big) return 4;
    if ((unsigned int)ulg != ubig) return 5;
    if (wrap != 0xffu) return 6;
    if (pack != 0xa5) return 7;
    if (sign_extend(0xffu) != -1) return 8;
    if (round != 44512) return 9;
    if (tot != 791530190) return 10;
    if ((int)(uint8_t)(big) != 0x78) return 11;
    if ((unsigned short)(big >> 16) != 0x1234u) return 12;

    return 0;
}
