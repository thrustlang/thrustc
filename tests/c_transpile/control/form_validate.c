static int valid_age(int age) {
    if (age < 0) return 0;
    if (age >= 18) {
        if (age > 130) return 0;
        return 2;
    } else if (age >= 13) {
        return 1;
    }
    return 0;
}

static int validate_field(int kind, int value) {
    switch (kind) {
        case 0:
            if (value == 0) return -1;
            /* fall through */
        case 1:
            return valid_age(value);
        case 2:
            return value % 3 == 0;
        default:
            return -2;
    }
}

static int validate_form(const int *kinds, const int *values, int n) {
    int score = 0, checked = 0;
    for (int i = 0; i < n; i++) {
        int result = validate_field(kinds[i], values[i]);
        if (result < 0) continue;
        if (result == 0) return -1;
        checked++;
        if (result == 2) score += 2;
        else if (result == 1) score += 1;
        else score += 0;
    }
    if (checked == 0) return -2;
    return score;
}

int form_validate_check(void) {
    int kinds[3] = {1, 0, 2};
    int values[3] = {25, 0, 9};
    if (validate_form(kinds, values, 3) != 3) return 1;
    int skipped[4] = {1, 1, 0, 3};
    int skipped_values[4] = {10, 5, 0, 1};
    if (validate_form(skipped, skipped_values, 2) != -1) return 2;
    if (validate_form(skipped + 2, skipped_values + 2, 2) != -2) return 3;
    if (valid_age(131) != 0 || valid_age(13) != 1) return 4;
    if (validate_field(2, 12) != 1 || validate_field(9, 1) != -2) return 5;
    return 0;
}
