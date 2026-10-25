#define LOOP_5(out) do { \
    int i; \
    int total = 0; \
    for (i = 0; i < 7; i++) { \
        if ((i % 1) == 0) continue; \
        total += i; \
        if (total > 21) break; \
    } \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    LOOP_5(out);
    int expected = 0;
    int i;
    for (i = 0; i < 7; i++) {
        if ((i % 1) == 0) continue;
        expected += i;
        if (expected > 21) break;
    }
    return out == expected ? 0 : 1;
}
