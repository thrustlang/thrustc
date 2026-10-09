static int row_sum(const int *row, int cols) {
    int total = 0;
    for (int i = 0; i < cols; i++) {
        total = total + row[i];
    }
    return total;
}

int matrix_trace_check(void) {
    int r0[3] = {1, 2, 3};
    int r1[3] = {4, 5, 6};
    int r2[3] = {7, 8, 9};
    int *rows[3] = {r0, r1, r2};
    int **grid = rows;

    int trace = 0;
    for (int i = 0; i < 3; i++) {
        trace += *(grid[i] + i);
    }
    if (trace != 15) { return 1; }

    int total = 0;
    int **rp = grid;
    for (int i = 0; i < 3; i++) {
        total += row_sum(*rp, 3);
        rp++;
    }
    if (total != 45) { return 2; }

    int *cell = &r1[1];
    *cell = *cell * 10;
    if (r1[1] != 50) { return 3; }

    int *walker = r2 + 2;
    int back = 0;
    for (int i = 0; i < 3; i++) {
        back = back * 10 + *walker;
        walker--;
    }
    if (back != 987) { return 4; }

    return 0;
}
