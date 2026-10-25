#define LOOP_3(out) do { \
    int i; \
    int total = 0; \
    for (i = 0; i < 6; i++) { \
        if ((i % 3) == 0) continue; \
        total += i; \
        if (total > 18) break; \
    } \
    (out) = total; \
} while (0)

int main(void) {
    int out = 0;
    LOOP_3(out);
    int expected = 0;
    int i;
    for (i = 0; i < 6; i++) {
        if ((i % 3) == 0) continue;
        expected += i;
        if (expected > 18) break;
    }
    return out == expected ? 0 : 1;
}
