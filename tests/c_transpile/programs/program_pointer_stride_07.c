static int stride_steps_07 = 0;

static int stride_merge_07(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_07 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {-1, 0, -5, 8, -4, 1, 3};
    int right[7] = {1, 11, -5, 3, -6, 7, -4};
    int out[7] = {0};
    int total = stride_merge_07(out, left, right, 7);
    if (stride_steps_07 != 7) return 1;
    if (out[3] != 8) return 2;
    if (total != 16) return 3;
    return 0;
}
