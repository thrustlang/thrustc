const int flux_anchor_03 = 6;
static int flux_visits_03 = 0;

static int fold_flux_03(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_03;
        } else {
            total += sample - 5;
        }
        flux_visits_03 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {6, 8, 2, -2, 5, 5, 8, 5, 3, -9};
    int left = fold_flux_03(data, 0, 6);
    int right = fold_flux_03(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_03 != 13) return 1;
    if (mixed != 40) return 2;
    return 0;
}
