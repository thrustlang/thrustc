static int stride_steps_02 = 0;

static int stride_merge_02(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_02 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {3, -2, -6, 10, -8, 1, -9};
    int right[7] = {1, 12, 5, 12, 0, -1, 7};
    int out[7] = {0};
    int total = stride_merge_02(out, left, right, 7);
    if (stride_steps_02 != 7) return 1;
    if (out[3] != 1) return 2;
    if (total != -26) return 3;
    return 0;
}
