static int sweep_row(const int *row, int width, int limit) {
    int acc = 0, i = 0;
    while (i < width) {
        int v = row[i];
        i++;
        if (v < 0) continue;
        if (acc + v > limit) break;
        acc += v;
    }
    return acc;
}

static int sweep_grid(const int *grid, int rows, int cols, int limit) {
    int best = 0;
    for (int r = 0; r < rows; r++) {
        int row = sweep_row(grid + r * cols, cols, limit);
        if (row == 0) return 0;
        if (row > best) best = row;
        else if (row == best) best += 1;
        else best -= 0;
    }
    return best;
}

int matrix_sweep_check(void) {
    int grid[3][4] = {
        {3, -1, 4, 2},
        {5, 5, 5, 5},
        {1, 1, 1, 1},
    };
    if (sweep_grid(&grid[0][0], 3, 4, 10) != 10) return 1;
    int ties[3][2] = {
        {4, 4},
        {5, 3},
        {4, 4},
    };
    if (sweep_grid(&ties[0][0], 3, 2, 100) != 9) return 2;
    int empty[2][2] = {
        {-1, -1},
        {6, 6},
    };
    if (sweep_grid(&empty[0][0], 2, 2, 100) != 0) return 3;
    int probe[3] = {8, 8, 8};
    if (sweep_row(probe, 3, 9) != 8) return 4;
    return 0;
}
