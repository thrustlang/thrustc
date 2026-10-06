const int flux_anchor_05 = 8;
static int flux_visits_05 = 0;

static int fold_flux_05(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_05;
        } else {
            total += sample - 3;
        }
        flux_visits_05 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {6, 3, 8, 4, 9, 11, 0, 2, 8, 12};
    int left = fold_flux_05(data, 0, 6);
    int right = fold_flux_05(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_05 != 13) return 1;
    if (mixed != 114) return 2;
    return 0;
}
