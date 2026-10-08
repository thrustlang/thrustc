#define DOUBLE2(x) ((x) * 2)
#define APPLY_DBL(y) do { (y) = DOUBLE2(y); } while (0)
int apply_dbl_four(void) {
    int k = 4;
    APPLY_DBL(k);
    return k;
}
