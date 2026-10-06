static int pointer_writes_03 = 0;

static int remap_pack_03(int *out, const int *src, int n) {
    int kept = 0;
    for (int i = 0; i < n; ++i) {
        int value = *(src + i);
        if (value >= 0) {
            *(out + kept) = value + i;
            kept += 1;
            pointer_writes_03 += 1;
        }
    }
    return kept;
}

static int sum_pack_03(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += *(values + i);
    return total;
}

int main(void) {
    int source[11] = {8, 5, -2, 12, -1, 9, -8, -8, 2, 2, -8};
    int out[11] = {0};
    int count = remap_pack_03(out, source, 11);
    int total = sum_pack_03(out, count);
    if (pointer_writes_03 != count) return 1;
    if (count != 6) return 2;
    if (total != 64) return 3;
    return 0;
}
