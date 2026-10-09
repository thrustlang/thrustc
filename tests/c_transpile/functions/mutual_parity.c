static int is_even(int n);
static int is_odd(int n);

static int is_even(int n) {
    if (n == 0) {
        return 1;
    }
    return is_odd(n - 1);
}

static int is_odd(int n) {
    if (n == 0) {
        return 0;
    }
    return is_even(n - 1);
}

static int collatz_steps(long long n) {
    int steps = 0;
    while (n > 1) {
        if (n % 2 == 0) {
            n = n / 2;
        } else {
            n = 3 * n + 1;
        }
        steps = steps + 1;
    }
    return steps;
}

static int count_even_pairs(int limit) {
    int total = 0;
    for (int i = 0; i < limit; i = i + 1) {
        if (is_even(i)) {
            total = total + 1;
        }
    }
    return total;
}

int mutual_parity_check(void) {
    if (is_even(10) != 1) { return 1; }
    if (is_odd(10) != 0) { return 2; }
    if (is_even(7) != 0) { return 3; }
    if (is_odd(7) != 1) { return 4; }
    if (collatz_steps(6) != 8) { return 5; }
    if (collatz_steps(27) != 111) { return 6; }
    if (count_even_pairs(10) != 5) { return 7; }
    return 0;
}
