static int stride_steps_08 = 0;

static int stride_merge_08(int *dst, const int *left, const int *right, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int lv = *(left + i);
        int rv = *(right + (n - 1 - i));
        *(dst + i) = lv - rv + i;
        total += *(dst + i);
        stride_steps_08 += 1;
    }
    return total;
}

int main(void) {
    int left[7] = {-5, 5, 6, 0, 1, 0, 7};
    int right[7] = {4, -6, -6, -3, -9, 5, 1};
    int out[7] = {0};
    int total = stride_merge_08(out, left, right, 7);
    if (stride_steps_08 != 7) return 1;
    if (out[3] != 6) return 2;
    if (total != 49) return 3;
    return 0;
}
