static int stride_steps_03 = 0;

static int stride_merge_03(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_03 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {-3, -9, 4, -4, 4, 3, 0};
    int right[7] = {12, 11, -6, 2, 11, -2, 8};
    int out[7] = {0};
    int total = stride_merge_03(out, left, right, 7);
    if (stride_steps_03 != 7) return 1;
    if (out[3] != -3) return 2;
    if (total != -20) return 3;
    return 0;
}
