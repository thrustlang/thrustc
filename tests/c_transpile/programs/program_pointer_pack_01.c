static int pointer_writes_01 = 0;

static int remap_pack_01(int *out, const int *src, int n) {
    int kept = 0;
    for (int i = 0; i < n; ++i) {
        int value = *(src + i);
        if (value >= 0) {
            *(out + kept) = value + i;
            kept += 1;
            pointer_writes_01 += 1;
        }
    }
    return kept;
}

static int sum_pack_01(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += *(values + i);
    return total;
}

int main(void) {
    int source[11] = {0, 5, -5, 2, -2, 0, 2, 12, 1, 6, 0};
    int out[11] = {0};
    int count = remap_pack_01(out, source, 11);
    int total = sum_pack_01(out, count);
    if (pointer_writes_01 != count) return 1;
    if (count != 9) return 2;
    if (total != 77) return 3;
    return 0;
}
