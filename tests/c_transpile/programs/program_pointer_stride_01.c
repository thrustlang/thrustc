static int stride_steps_01 = 0;

static int stride_merge_01(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_01 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {2, -7, -6, 4, 9, 4, 11};
    int right[7] = {-2, 11, -2, 6, 7, -6, 2};
    int out[7] = {0};
    int total = stride_merge_01(out, left, right, 7);
    if (stride_steps_01 != 7) return 1;
    if (out[3] != 1) return 2;
    if (total != 22) return 3;
    return 0;
}
