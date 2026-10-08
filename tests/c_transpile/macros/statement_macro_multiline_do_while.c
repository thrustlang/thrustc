#define MLSET(x) do { \
    (x) = 1; \
    (x) = (x) + 2; \
} while (0)
int mlset_three(void) {
    int k = 0;
    MLSET(k);
    return k;
}
