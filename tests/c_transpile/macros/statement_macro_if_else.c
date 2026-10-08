#define ABSSET(x) do { if ((x) < 0) { (x) = -(x); } else { (x) = (x); } } while (0)
int abs_neg_seven(void) {
    int k = -7;
    ABSSET(k);
    return k;
}
