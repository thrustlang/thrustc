static int pointer_writes_07 = 0;

static int remap_pack_07(int *out, const int *src, int n) {
    int kept = 0;
    for (int i = 0; i < n; ++i) {
        int value = *(src + i);
        if (value >= 0) {
            *(out + kept) = value + i;
            kept += 1;
            pointer_writes_07 += 1;
        }
    }
    return kept;
}

static int sum_pack_07(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += *(values + i);
    return total;
}

int main(void) {
    int source[11] = {12, 2, 3, 7, 6, 4, -3, -7, 2, 5, 5};
    int out[11] = {0};
    int count = remap_pack_07(out, source, 11);
    int total = sum_pack_07(out, count);
    if (pointer_writes_07 != count) return 1;
    if (count != 9) return 2;
    if (total != 88) return 3;
    return 0;
}
