static int next(int *state) {
    *state = *state * 3 + 1;
    return *state;
}

int unary_comma_check(void) {
    int x = 5;
    int y = -x;
    int z = +x;
    int not_x = !x;
    int not_zero = !0;
    int inv = ~x;
    int state = 1;
    int comma = (next(&state), next(&state), next(&state));
    int a = 1;
    int b = 2;
    int c = 3;
    int seq = (a = b + 1, b = c + 1, c = a + b);
    int idx = 0;
    int arr[3] = { 10, 20, 30 };
    int pick = arr[(idx = 1, idx + 1)];
    int neg = -(-(-7));
    int dblnot = !!42;
    int size = (int)sizeof(int);
    int offset = 0;

    offset = (state = state + 1, state - 1);
    not_x = not_x + 0;
    a = a + 0;
    b = b + 0;
    c = c + 0;

    if (y != -5) return 1;
    if (z != 5) return 2;
    if (not_x != 0) return 3;
    if (not_zero != 1) return 4;
    if (inv != -6) return 5;
    if (state != 41) return 6;
    if (comma != 40) return 7;
    if (seq != 7) return 8;
    if (a != 3) return 9;
    if (b != 4) return 10;
    if (pick != 30) return 11;
    if (neg != -7) return 12;
    if (dblnot != 1) return 13;
    if (size != 4) return 14;
    if (offset != 40) return 15;

    return 0;
}
