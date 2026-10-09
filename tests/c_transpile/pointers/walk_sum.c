static int walk_sum(const int *xs, int n) {
    int total = 0;
    for (int i = 0; i < n; i++) {
        total = total + *(xs + i);
    }
    return total;
}

static int stride_sum(const int *xs, int n, int step) {
    int total = 0;
    for (int i = 0; i < n; i += step) {
        total += xs[i];
    }
    return total;
}

static int index_sum(const int *xs, int n) {
    int total = 0;
    for (int i = 0; i < n; i++) {
        total = total + xs[i];
    }
    return total;
}

int walk_sum_check(void) {
    int data[8] = {2, 4, 6, 8, 10, 12, 14, 16};
    int *p = data;
    int n = (int)(sizeof(data) / sizeof(data[0]));

    if (walk_sum(data, n) != 72) { return 1; }
    if (index_sum(p, n) != 72) { return 2; }
    if (stride_sum(data, n, 2) != 32) { return 3; }

    int total = 0;
    for (int i = 0; i < n; ++i) {
        total = total + p[i];
    }
    if (total != 72) { return 4; }

    int tail = 0;
    for (int i = 4; i < 8; i++) {
        tail += data[i];
    }
    if (tail != 52) { return 5; }
    if (data[2] != 6) { return 6; }

    return 0;
}
