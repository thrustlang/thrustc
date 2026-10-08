#define SUM2(a, b, r) do { int t1 = (a), t2 = (b); (r) = t1 + t2; } while (0)
int sum_two(void) {
    int r = 0;
    SUM2(5, 6, r);
    return r;
}
