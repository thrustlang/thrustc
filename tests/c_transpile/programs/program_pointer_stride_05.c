static int stride_steps_05 = 0;

static int stride_merge_05(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_05 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {8, 12, 5, -7, -1, 6, 2};
    int right[7] = {8, -3, 12, 3, 12, -8, 12};
    int out[7] = {0};
    int total = stride_merge_05(out, left, right, 7);
    if (stride_steps_05 != 7) return 1;
    if (out[3] != -7) return 2;
    if (total != 10) return 3;
    return 0;
}
