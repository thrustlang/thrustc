static int stride_steps_06 = 0;

static int stride_merge_06(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_06 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {-2, -8, -4, 12, 10, 11, 8};
    int right[7] = {-1, 1, -7, -3, -2, -3, 9};
    int out[7] = {0};
    int total = stride_merge_06(out, left, right, 7);
    if (stride_steps_06 != 7) return 1;
    if (out[3] != 18) return 2;
    if (total != 54) return 3;
    return 0;
}
