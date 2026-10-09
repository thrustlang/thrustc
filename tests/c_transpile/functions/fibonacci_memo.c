static int fib(int n);

static int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

static long long fib_iter(int n) {
    long long a = 0;
    long long b = 1;
    for (int i = 0; i < n; i = i + 1) {
        long long next = a + b;
        a = b;
        b = next;
    }
    return a;
}

static int fib_sum(int limit) {
    int total = 0;
    for (int i = 0; i <= limit; i = i + 1) {
        total = total + fib(i);
    }
    return total;
}

static int fib_index_of(long long target) {
    int i = 0;
    while (fib_iter(i) < target) {
        i = i + 1;
    }
    return i;
}

int fibonacci_memo_check(void) {
    if (fib(0) != 0) { return 1; }
    if (fib(10) != 55) { return 2; }
    if (fib(15) != 610) { return 3; }
    if (fib_iter(20) != 6765) { return 4; }
    if (fib_sum(6) != 20) { return 5; }
    if (fib_index_of(34) != 9) { return 6; }
    return 0;
}
