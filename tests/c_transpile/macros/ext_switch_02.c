#define SWITCH_2(x, out) do { \
    int score = 0; \
    switch (x) { \
        case 0: score = 10; break; \
        case 1: \
        case 2: score = 20; \
        case 3: score += 5; break; \
        case 5: score = 30; break; \
        case 7: score = 40; break; \
        default: score = 50; \
    } \
    (out) = score; \
} while (0)

int main(void) {
    int out = 0;
    SWITCH_2(2, out);
    return out == 25 ? 0 : 1;
}
