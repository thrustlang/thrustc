static int stride_steps_09 = 0;

static int stride_merge_09(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_09 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {1, -3, 2, -6, -6, 11, 6};
    int right[7] = {-6, -2, 2, 9, 0, 10, -5};
    int out[7] = {0};
    int total = stride_merge_09(out, left, right, 7);
    if (stride_steps_09 != 7) return 1;
    if (out[3] != -12) return 2;
    if (total != 18) return 3;
    return 0;
}
