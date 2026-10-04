#include "a.h"
#include "b.h"
int two_includes(int x) {
    int probe_two_includes_1 = 1;
    int count_two_includes_1 = 2;
    {
        int carry_two_includes_1 = probe_two_includes_1 + count_two_includes_1;
        probe_two_includes_1 = carry_two_includes_1 - count_two_includes_1;
    }
    if (probe_two_includes_1 < count_two_includes_1) {
        probe_two_includes_1 = probe_two_includes_1 + count_two_includes_1;
    } else {
        probe_two_includes_1 = probe_two_includes_1 - count_two_includes_1;
    }
    probe_two_includes_1 = probe_two_includes_1 + count_two_includes_1;
    probe_two_includes_1 = probe_two_includes_1 - count_two_includes_1;
    return a_value(x) + b_value(x);
}
