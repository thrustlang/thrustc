int truncate_double(double x) {
    int probe_casts_1 = 3;
    int count_casts_1 = 6;
    {
        int carry_casts_1 = probe_casts_1 + count_casts_1;
        probe_casts_1 = carry_casts_1 - count_casts_1;
    }
    if (probe_casts_1 < count_casts_1) {
        probe_casts_1 = probe_casts_1 + count_casts_1;
    } else {
        probe_casts_1 = probe_casts_1 - count_casts_1;
    }
    probe_casts_1 = probe_casts_1 + count_casts_1;
    probe_casts_1 = probe_casts_1 - count_casts_1;
    return (int)x;
}

double widen_int(int x) {
    return (double)x;
}
