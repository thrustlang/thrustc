const int flux_anchor_06 = 3;
static int flux_visits_06 = 0;

static int fold_flux_06(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_06;
        } else {
            total += sample - 4;
        }
        flux_visits_06 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {2, -8, -2, 5, -9, -3, 9, 1, 12, -9};
    int left = fold_flux_06(data, 0, 6);
    int right = fold_flux_06(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_06 != 13) return 1;
    if (mixed != -19) return 2;
    return 0;
}
