static void swap_ints(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

static void swap_ptrs(int **a, int **b) {
    int *tmp = *a;
    *a = *b;
    *b = tmp;
}

static void reverse_in_place(int *xs, int n) {
    int *lo = xs;
    int *hi = xs + n - 1;
    while (lo < hi) {
        swap_ints(lo, hi);
        lo++;
        hi--;
    }
}

int swap_pair_check(void) {
    int a = 3;
    int b = 7;
    int *pa = &a;
    int *pb = &b;

    swap_ints(pa, pb);
    if (a != 7 || b != 3) { return 1; }

    swap_ptrs(&pa, &pb);
    if (*pa != 3 || *pb != 7) { return 2; }
    if (pa != &b || pb != &a) { return 3; }

    **&pa = 11;
    if (b != 11) { return 4; }

    int xs[6] = {1, 2, 3, 4, 5, 6};
    reverse_in_place(xs, 6);
    if (xs[0] != 6 || xs[5] != 1) { return 5; }
    if (xs[2] != 4 || xs[3] != 3) { return 6; }

    int *walk = xs;
    int sum = 0;
    for (int i = 0; i < 6; i++) {
        sum = sum + *walk++;
    }
    if (sum != 21) { return 7; }

    return 0;
}
