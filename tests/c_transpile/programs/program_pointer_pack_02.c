static int pointer_writes_02 = 0;

static int remap_pack_02(int *out, const int *src, int n) {
    int kept = 0;
    for (int i = 0; i < n; ++i) {
        int value = *(src + i);
        if (value >= 0) {
            *(out + kept) = value + i;
            kept += 1;
            pointer_writes_02 += 1;
        }
    }
    return kept;
}

static int sum_pack_02(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += *(values + i);
    return total;
}

int main(void) {
    int source[11] = {-3, 11, 3, -9, -8, 3, 9, 5, -8, -9, -4};
    int out[11] = {0};
    int count = remap_pack_02(out, source, 11);
    int total = sum_pack_02(out, count);
    if (pointer_writes_02 != count) return 1;
    if (count != 5) return 2;
    if (total != 52) return 3;
    return 0;
}
