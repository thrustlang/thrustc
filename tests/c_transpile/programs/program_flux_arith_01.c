const int flux_anchor_01 = 4;
static int flux_visits_01 = 0;

static int fold_flux_01(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_01;
        } else {
            total += sample - 3;
        }
        flux_visits_01 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {-8, -3, -7, 8, 3, -4, 4, 2, 9, 11};
    int left = fold_flux_01(data, 0, 6);
    int right = fold_flux_01(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_01 != 13) return 1;
    if (mixed != 25) return 2;
    return 0;
}
