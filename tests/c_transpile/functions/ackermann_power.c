static int ackermann(int m, int n) {
    if (m == 0) {
        return n + 1;
    }
    if (n == 0) {
        return ackermann(m - 1, 1);
    }
    return ackermann(m - 1, ackermann(m, n - 1));
}

static int power(int base, int exp) {
    if (exp == 0) {
        return 1;
    }
    return base * power(base, exp - 1);
}

static void stats(const int *data, int len, int *min_out, int *max_out, int *sum_out) {
    int lo = data[0];
    int hi = data[0];
    int total = 0;
    for (int i = 0; i < len; i = i + 1) {
        if (data[i] < lo) {
            lo = data[i];
        }
        if (data[i] > hi) {
            hi = data[i];
        }
        total = total + data[i];
    }
    *min_out = lo;
    *max_out = hi;
    *sum_out = total;
}

static int ackermann_band(int m, int lo, int hi) {
    int total = 0;
    for (int n = lo; n <= hi; n = n + 1) {
        total = total + ackermann(m, n);
    }
    return total;
}
int ackermann_power_check(void) {
    if (ackermann(0, 5) != 6) { return 1; }
    if (ackermann(1, 5) != 7) { return 2; }
    if (ackermann(2, 3) != 9) { return 3; }
    if (ackermann(3, 3) != 61) { return 4; }
    if (power(2, 10) != 1024) { return 5; }
    if (power(3, 4) != 81) { return 6; }
    int data[6] = {7, -3, 19, 4, 0, 11};
    int lo = 0;
    int hi = 0;
    int total = 0;
    stats(data, 6, &lo, &hi, &total);
    if (lo != -3) { return 7; }
    if (hi != 19) { return 8; }
    if (total != 38) { return 9; }
    if (ackermann_band(2, 0, 3) != 3 + 5 + 7 + 9) { return 10; }
    return 0;
}
