static int pointer_writes_08 = 0;

static int remap_pack_08(int *out, const int *src, int n) {
    int kept = 0;
    for (int i = 0; i < n; ++i) {
        int value = *(src + i);
        if (value >= 0) {
            *(out + kept) = value + i;
            kept += 1;
            pointer_writes_08 += 1;
        }
    }
    return kept;
}

static int sum_pack_08(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += *(values + i);
    return total;
}

int main(void) {
    int source[11] = {-1, 6, -9, -4, 4, -7, -7, 9, 2, -7, -7};
    int out[11] = {0};
    int count = remap_pack_08(out, source, 11);
    int total = sum_pack_08(out, count);
    if (pointer_writes_08 != count) return 1;
    if (count != 4) return 2;
    if (total != 41) return 3;
    return 0;
}
