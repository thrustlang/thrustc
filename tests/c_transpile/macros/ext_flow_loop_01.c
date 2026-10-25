#define LOOP_1(out) do { \
    int i; \
    int total = 0; \
    for (i = 0; i < 5; i++) { \
        if ((i % 1) == 0) continue; \
        total += i; \
        if (total > 15) break; \
    } \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    LOOP_1(out);
    int expected = 0;
    int i;
    for (i = 0; i < 5; i++) {
        if ((i % 1) == 0) continue;
        expected += i;
        if (expected > 15) break;
    }
    return out == expected ? 0 : 1;
}
