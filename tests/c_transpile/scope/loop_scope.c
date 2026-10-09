static int loop_scope_tally = 0;

static int sum_range(int start, int stop) {
    int acc = 0;
    for (int i = start; i < stop; i++) {
        int i_shadow = i * 2;
        acc += i_shadow;
    }
    int i = 100;
    for (int i = 0; i < 3; i++) {
        acc += i;
    }
    return acc + i;
}

static int filter_scan(int limit) {
    int kept = 0;
    for (int n = 1; n <= limit; n++) {
        if (n % 2 == 0) {
            int doubled = n * 2;
            kept += doubled;
        }
        int viewed = n;
        {
            int n = viewed + 1;
            loop_scope_tally += (n > 2) ? 1 : 0;
        }
    }
    return kept;
}

static int grid_walk(int rows, int cols) {
    int sum = 0;
    for (int r = 0; r < rows; r++) {
        int row_base = r * cols;
        for (int c = 0; c < cols; c++) {
            int cell = row_base + c;
            sum += cell;
        }
    }
    return sum;
}

int loop_scope_check(void) {
    loop_scope_tally = 0;
    if (sum_range(1, 4) != 115) { return 1; }
    if (sum_range(0, 2) != 105) { return 2; }
    if (filter_scan(5) != 12) { return 3; }
    if (loop_scope_tally != 4) { return 4; }
    if (grid_walk(3, 4) != 66) { return 5; }
    if (grid_walk(2, 2) != 6) { return 6; }
    return 0;
}
