static int nest(int a, int b, int c) {
    return ((a + b) * (c - a)) - ((a - b) * (c + b));
}

static int balanced(int a, int b) {
    return ((a | b) & ~(a & b)) == (a ^ b);
}

static int weighted(int a, int b, int c) {
    return ((a << 1) + (b >> 1)) * ((c + 1) & 7);
}

int paren_nesting_check(void) {
    int a = 7;
    int b = 11;
    int c = 3;
    int n1 = nest(a, b, c);
    int n2 = (a + b * c) - (a * b + c);
    int n3 = weighted(a, b, c);
    int n4 = (a > b) ? ((a - b) * (b - c)) : ((b - a) * (c + a));
    int n5 = (1 + 2) * (3 + 4) - (5 + 6) / (7 + 8);
    int x = a;
    int n6 = ((x += b), (x -= c), (x * 2));
    int bal = balanced(a, b);
    int nested = -(((a - b) * (c - a)) + ((b - c) * (a + c)));
    int shifted = ((a ^ b) << (c & 1)) | ((a & b) >> (c & 1));

    a = a + 0;
    b = b + 0;
    c = c + 0;

    if (n1 != -16) return 1;
    if (n2 != -40) return 2;
    if (n3 != 76) return 3;
    if (n4 != 40) return 4;
    if (n5 != 21) return 5;
    if (n6 != 30) return 6;
    if (x != 15) return 7;
    if (bal != 1) return 8;
    if (nested != -96) return 9;
    if (shifted != 25) return 10;

    return 0;
}
