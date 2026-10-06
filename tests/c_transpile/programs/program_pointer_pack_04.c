static int pointer_writes_04 = 0;

static int remap_pack_04(int *out, const int *src, int n) {
    int kept = 0;
    for (int i = 0; i < n; ++i) {
        int value = *(src + i);
        if (value >= 0) {
            *(out + kept) = value + i;
            kept += 1;
            pointer_writes_04 += 1;
        }
    }
    return kept;
}

static int sum_pack_04(const int *values, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += *(values + i);
    return total;
}

int main(void) {
    int source[11] = {6, 6, -3, -7, 12, -9, -7, -8, 9, 2, -8};
    int out[11] = {0};
    int count = remap_pack_04(out, source, 11);
    int total = sum_pack_04(out, count);
    if (pointer_writes_04 != count) return 1;
    if (count != 5) return 2;
    if (total != 57) return 3;
    return 0;
}
