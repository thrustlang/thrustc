static int pointer_writes_10 = 0;

static int remap_pack_10(int *out, const int *src, int n) {
    int kept = 0;
    for (int i = 0; i < n; ++i) {
        int value = *(src + i);
        if (value >= 0) {
            *(out + kept) = value + i;
            kept += 1;
            pointer_writes_10 += 1;
        }
    }
    return kept;
}

static int sum_pack_10(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += *(values + i);
    return total;
}

int main(void) {
    int source[11] = {11, 8, 2, 2, 7, 9, -8, -5, 4, 4, 4};
    int out[11] = {0};
    int count = remap_pack_10(out, source, 11);
    int total = sum_pack_10(out, count);
    if (pointer_writes_10 != count) return 1;
    if (count != 9) return 2;
    if (total != 93) return 3;
    return 0;
}
