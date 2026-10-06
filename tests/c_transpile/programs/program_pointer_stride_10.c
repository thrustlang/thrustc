static int stride_steps_10 = 0;

static int stride_merge_10(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_10 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {8, 12, -1, -9, 1, 0, 2};
    int right[7] = {4, 0, 10, -5, 4, -1, -5};
    int out[7] = {0};
    int total = stride_merge_10(out, left, right, 7);
    if (stride_steps_10 != 7) return 1;
    if (out[3] != -1) return 2;
    if (total != 27) return 3;
    return 0;
}
