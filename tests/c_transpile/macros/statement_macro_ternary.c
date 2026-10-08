#define SELECT(tag, a, b, out) do { int chosen = ((tag) ? (a) : (b)); (out) = chosen; } while (0)
int select_high(void) {
    int r = 0;
    SELECT(1, 10, 20, r);
    return r;
}
