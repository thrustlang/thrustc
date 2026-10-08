#define COUNTDOWN(n) do { while ((n) > 0) { (n) = (n) - 1; } } while (0)
int countdown_five(void) {
    int k = 5;
    COUNTDOWN(k);
    return k;
}
