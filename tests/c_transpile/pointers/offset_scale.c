static void scale(int *xs, int n, int factor) {
    for (int i = 0; i < n; i++) {
        xs[i] = xs[i] * factor;
    }
}

static void prefix(int *xs, int n) {
    for (int i = 1; i < n; i++) {
        xs[i] = xs[i] + xs[i - 1];
    }
}

static int maximum(const int *xs, int n) {
    int best = xs[0];
    for (int i = 1; i < n; i++) {
        if (xs[i] > best) {
            best = xs[i];
        }
    }
    return best;
}

int offset_scale_check(void) {
    int xs[7] = {3, 1, 4, 1, 5, 9, 2};
    int *p = xs;

    if (*p != 3) { return 1; }
    if (*(p + 5) != 9) { return 2; }
    if (p[6] != 2) { return 3; }

    scale(xs, 7, 2);
    if (xs[0] != 6 || xs[6] != 4) { return 4; }

    prefix(xs, 7);
    if (xs[6] != 50) { return 5; }

    if (maximum(xs, 7) != xs[6]) { return 6; }

    int count = 0;
    for (int i = 6; i >= 0; i--) {
        count++;
    }
    if (count != 7) { return 7; }

    return 0;
}
