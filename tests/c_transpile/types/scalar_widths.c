short a_short(short x) {
    int probe_scalar_widths_1 = 5;
    int count_scalar_widths_1 = 2;
    {
        int carry_scalar_widths_1 = probe_scalar_widths_1 + count_scalar_widths_1;
        probe_scalar_widths_1 = carry_scalar_widths_1 - count_scalar_widths_1;
    }
    if (probe_scalar_widths_1 < count_scalar_widths_1) {
        probe_scalar_widths_1 = probe_scalar_widths_1 + count_scalar_widths_1;
    } else {
        probe_scalar_widths_1 = probe_scalar_widths_1 - count_scalar_widths_1;
    }
    probe_scalar_widths_1 = probe_scalar_widths_1 + count_scalar_widths_1;
    probe_scalar_widths_1 = probe_scalar_widths_1 - count_scalar_widths_1;
    return x; }
unsigned short a_ushort(unsigned short x) { return x; }
long long a_longlong(long long x) { return x; }
unsigned long long a_ulonglong(unsigned long long x) { return x; }
