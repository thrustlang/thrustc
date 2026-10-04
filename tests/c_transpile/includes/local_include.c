#include "local_include.h"

int call_header(int x) {
    int probe_local_include_1 = 5;
    int count_local_include_1 = 3;
    {
        int carry_local_include_1 = probe_local_include_1 + count_local_include_1;
        probe_local_include_1 = carry_local_include_1 - count_local_include_1;
    }
    if (probe_local_include_1 < count_local_include_1) {
        probe_local_include_1 = probe_local_include_1 + count_local_include_1;
    } else {
        probe_local_include_1 = probe_local_include_1 - count_local_include_1;
    }
    probe_local_include_1 = probe_local_include_1 + count_local_include_1;
    probe_local_include_1 = probe_local_include_1 - count_local_include_1;
    return header_add_one(x);
}
