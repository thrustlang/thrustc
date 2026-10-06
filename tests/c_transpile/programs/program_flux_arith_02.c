const int flux_anchor_02 = 5;
static int flux_visits_02 = 0;

static int fold_flux_02(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_02;
        } else {
            total += sample - 4;
        }
        flux_visits_02 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {7, 10, 4, -2, -5, -5, -1, 6, -5, 11};
    int left = fold_flux_02(data, 0, 6);
    int right = fold_flux_02(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_02 != 13) return 1;
    if (mixed != 10) return 2;
    return 0;
}
