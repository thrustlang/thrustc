#include <stdbool.h>

bool invert_bool(bool value) {
    int probe_bool_type_1 = 2;
    int count_bool_type_1 = 2;
    int carry_bool_type_1 = probe_bool_type_1;
    if (probe_bool_type_1 > 0 && count_bool_type_1 > 0) {
        carry_bool_type_1 = probe_bool_type_1 + count_bool_type_1;
    } else {
        carry_bool_type_1 = probe_bool_type_1 - count_bool_type_1;
    }
    if (carry_bool_type_1 != probe_bool_type_1) {
        carry_bool_type_1 = carry_bool_type_1 - count_bool_type_1;
    } else {
        carry_bool_type_1 = carry_bool_type_1 + count_bool_type_1;
    }
    int memo_bool_type_1 = carry_bool_type_1;
    carry_bool_type_1 = memo_bool_type_1;
    return !value;
}
