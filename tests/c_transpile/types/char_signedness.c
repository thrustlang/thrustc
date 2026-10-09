#include <stdbool.h>

typedef char byte_t;
typedef signed char schar_t;
typedef unsigned char uchar_t;
typedef byte_t block_t[8];

static int digit_value(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    return -1;
}

static bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static int checksum_block(const block_t block, int n) {
    int acc = 0;
    for (int i = 0; i < n; i = i + 1) {
        uchar_t u = (uchar_t)block[i];
        acc = (acc + u) & 0xFF;
    }
    return acc;
}

static int count_bytes(const uchar_t *data, int n, uchar_t target) {
    int hits = 0;
    for (int i = 0; i < n; i++) {
        if (data[i] == target) hits++;
    }
    return hits;
}

int char_signedness_check(void) {
    block_t text = {'T', 'h', 'r', 'u', 's', 't', '!', 0};
    if (digit_value('7') != 7) return 1;
    if (digit_value('x') != -1) return 2;
    if (!is_alpha('Q')) return 3;
    if (is_alpha('3')) return 4;
    if (checksum_block(text, 7) != (int)(('T' + 'h' + 'r' + 'u' + 's' + 't' + '!') & 0xFF)) return 5;

    uchar_t raw[5] = {0u, 255u, 255u, 1u, 255u};
    if (count_bytes(raw, 5, (uchar_t)255) != 3) return 6;
    if (count_bytes(raw, 5, 0u) != 1) return 7;

    uchar_t high = (uchar_t)200;
    int as_int = high;
    if (as_int != 200) return 8;

    schar_t neg = (schar_t)-1;
    if (neg >= 0) return 9;

    const char *msg = "thrust";
    int len = 0;
    while (msg[len] != '\0') len++;
    if (len != 6) return 10;

    return 0;
}
