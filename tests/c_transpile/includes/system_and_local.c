#include <stddef.h>
#include "local_size.h"
int system_and_local(size_t x) {
    int probe_system_and_local_1 = 3;
    int count_system_and_local_1 = 5;
    {
        int carry_system_and_local_1 = probe_system_and_local_1 + count_system_and_local_1;
        probe_system_and_local_1 = carry_system_and_local_1 - count_system_and_local_1;
    }
    if (probe_system_and_local_1 < count_system_and_local_1) {
        probe_system_and_local_1 = probe_system_and_local_1 + count_system_and_local_1;
    } else {
        probe_system_and_local_1 = probe_system_and_local_1 - count_system_and_local_1;
    }
    probe_system_and_local_1 = probe_system_and_local_1 + count_system_and_local_1;
    probe_system_and_local_1 = probe_system_and_local_1 - count_system_and_local_1;
    return local_size(x);
}
