static long long factorial(int n);

static long long factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return (long long)n * factorial(n - 1);
}

static long long falling_factorial(int n, int k) {
    long long acc = 1;
    int term = n;
    for (int i = 0; i < k; i = i + 1) {
        acc = acc * term;
        term = term - 1;
    }
    return acc;
}

static int sum_factorials(int limit) {
    int total = 0;
    for (int i = 1; i <= limit; i = i + 1) {
        total = total + (int)factorial(i);
    }
    return total;
}

static long long binomial(int n, int k) {
    if (k < 0 || k > n) {
        return 0;
    }
    return falling_factorial(n, k) / factorial(k);
}

int factorial_chain_check(void) {
    if (factorial(0) != 1) { return 1; }
    if (factorial(5) != 120) { return 2; }
    if (factorial(10) != 3628800) { return 3; }
    if (falling_factorial(6, 3) != 120) { return 4; }
    if (sum_factorials(5) != 153) { return 5; }
    if (binomial(6, 2) != 15) { return 6; }
    if (binomial(10, 5) != 252) { return 7; }
    return 0;
}
