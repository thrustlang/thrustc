static int gcd(int a, int b) {
    if (b == 0) {
        return a;
    }
    return gcd(b, a % b);
}

static int lcm(int a, int b) {
    return (a / gcd(a, b)) * b;
}

static int gcd_three(int a, int b, int c) {
    return gcd(gcd(a, b), c);
}

static void reduce_fraction(int *num, int *den) {
    int g = gcd(*num, *den);
    *num = *num / g;
    *den = *den / g;
}

static int add_fractions(int n1, int d1, int n2, int d2, int *out_num, int *out_den) {
    int common = lcm(d1, d2);
    int left = n1 * (common / d1);
    int right = n2 * (common / d2);
    *out_num = left + right;
    *out_den = common;
    reduce_fraction(out_num, out_den);
    return common;
}

int gcd_lcm_check(void) {
    if (gcd(48, 36) != 12) { return 1; }
    if (gcd(270, 192) != 6) { return 2; }
    if (lcm(4, 6) != 12) { return 3; }
    if (lcm(21, 6) != 42) { return 4; }
    if (gcd_three(24, 36, 60) != 12) { return 5; }
    int num = 0;
    int den = 0;
    int common = add_fractions(1, 2, 1, 3, &num, &den);
    if (common != 6) { return 6; }
    if (num != 5) { return 7; }
    if (den != 6) { return 8; }
    reduce_fraction(&num, &den);
    if (num != 5) { return 9; }
    if (den != 6) { return 10; }
    return 0;
}
