struct Grid {
    int cells[2][3];
    int size;
};

#define SET_GRID(grid, row, col, value) do { \
    struct Grid *target = (grid); \
    target->cells[row][col] = (value); \
} while (0)

#define ADD_GRID(grid, row, col, value) do { \
    struct Grid *target = (grid); \
    int old = target->cells[row][col]; \
    target->cells[row][col] = old + (value); \
} while (0)

int main(void) {
    struct Grid grid = {{{0, 0, 0}, {0, 0, 0}}, 0};
    SET_GRID(&grid, 0, 2, 8);
    SET_GRID(&grid, 1, 1, 3);
    ADD_GRID(&grid, 0, 2, 4);
    if (grid.cells[0][2] != 12) return 1;
    if (grid.cells[1][1] != 3) return 2;
    int *cell = &grid.cells[1][1];
    if (*cell != 3) return 3;
    return 0;
}
