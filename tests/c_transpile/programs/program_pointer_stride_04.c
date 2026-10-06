static int stride_steps_04 = 0;

static int stride_merge_04(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_04 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {10, 6, 11, -7, 7, 0, 3};
    int right[7] = {11, -8, -2, 6, -8, -4, 5};
    int out[7] = {0};
    int total = stride_merge_04(out, left, right, 7);
    if (stride_steps_04 != 7) return 1;
    if (out[3] != -10) return 2;
    if (total != 51) return 3;
    return 0;
}
