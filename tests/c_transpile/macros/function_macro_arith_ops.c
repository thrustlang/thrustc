#define MULADD(a, b, c) ((a) * (b) + (c))
int arith_ops(int x) {
    return MULADD(x, 3, 4);
}
