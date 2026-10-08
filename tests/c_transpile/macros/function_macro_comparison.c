#define CLAMP10(x) ((x) > 10 ? 10 : (x))
int clamp_sum(void) {
    return CLAMP10(25) + CLAMP10(7);
}
