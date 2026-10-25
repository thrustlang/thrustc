#define LOOP_4(out) do { \
    int i; \
    int total = 0; \
    for (i = 0; i < 10; i++) { \
        if ((i % 2) == 0) continue; \
        total += i; \
        if (total > 30) break; \
    } \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    LOOP_4(out);
    int expected = 0;
    int i;
    for (i = 0; i < 10; i++) {
        if ((i % 2) == 0) continue;
        expected += i;
        if (expected > 30) break;
    }
    return out == expected ? 0 : 1;
}
