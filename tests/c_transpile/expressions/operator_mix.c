static int mix_precedence(int a, int b, int c) {
    return a + b * c - a / b + c % 3;
}

static int mix_shift_arith(int seed) {
    int acc = seed;
    acc = acc * 2 + 7;
    acc = acc - acc / 3;
    acc = (acc % 11) * 3 + 1;
    acc += (acc << 1) - (acc >> 2);
    return acc;
}

static int mix_fold(int n) {
    int total = 0;
    int i;
    for (i = 1; i <= n; i++) {
        total += (i * i - i) / 2 + (i % 3);
    }
    return total;
}

int operator_mix_check(void) {
    int a = 14;
    int b = 5;
    int c = -3;
    int sum = a + b * c;
    int ratio = (a + b) * c / 2;
    int mask = (a ^ b) & (a | c);
    int flag = (sum > ratio) != (a < b);
    int d = a;

    d += b;
    d -= c;
    d *= 2;
    d /= 3;
    d %= 7;
    d <<= 2;
    d >>= 1;
    d ^= mask & 0xff;
    d |= 1 << 4;
    d &= ~(1 << 3);

    if (sum != -1) return 1;
    if (ratio != -28) return 2;
    if (a % b != 4) return 3;
    if (a / b != 2) return 4;
    if (-a / 2 != -7) return 5;
    if (mix_precedence(6, 7, 8) != 64) return 6;
    if (mix_shift_arith(9) != 53) return 7;
    if (mix_fold(5) != 26) return 8;
    if (flag != 1) return 9;
    if ((d ^ (d & -1)) != 0) return 10;

    return 0;
}
