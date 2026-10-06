const int flux_anchor_08 = 5;
static int flux_visits_08 = 0;

static int fold_flux_08(const int *values, int start, int count) {
    int total = 0;
    for (int i = start; i < start + count; ++i) {
        int sample = values[i];
        if ((i % 2) == 0) {
            total += sample + flux_anchor_08;
        } else {
            total += sample - 2;
        }
        flux_visits_08 += 1;
    }
    return total;
}

int main(void) {
    int data[10] = {7, 5, 1, -7, -6, -8, 12, -5, 12, 9};
    int left = fold_flux_08(data, 0, 6);
    int right = fold_flux_08(data, 3, 7);
    int mixed = left + right;

    if (flux_visits_08 != 13) return 1;
    if (mixed != 15) return 2;
    return 0;
}
