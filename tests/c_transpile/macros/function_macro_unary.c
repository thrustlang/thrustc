#define NEGATE(x) (-(x))
#define SZINT() ((int)sizeof(int))
int negate_plus_size(void) {
    return NEGATE(5) + SZINT();
}
