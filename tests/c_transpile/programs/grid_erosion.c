#define GRID_ROWS 4
#define GRID_COLS 5
#define EROSION_STRENGTH 2
#define BEDROCK_LEVEL 1

struct Grid {
    int cells[GRID_ROWS][GRID_COLS];
    int steps;
};

const int seepage = 1;
static int eroded_cells = 0;

static int neighbor_min(const struct Grid *grid, int row, int col) {
    int best = grid->cells[row][col];
    if (row > 0 && grid->cells[row - 1][col] < best) best = grid->cells[row - 1][col];
    if (row + 1 < GRID_ROWS && grid->cells[row + 1][col] < best) best = grid->cells[row + 1][col];
    if (col > 0 && grid->cells[row][col - 1] < best) best = grid->cells[row][col - 1];
    if (col + 1 < GRID_COLS && grid->cells[row][col + 1] < best) best = grid->cells[row][col + 1];
    return best;
}

static void erode_step(struct Grid *grid) {
    for (int r = 0; r < GRID_ROWS; ++r)
        for (int c = 0; c < GRID_COLS; ++c) {
            int floor = neighbor_min(grid, r, c);
            int drop = (grid->cells[r][c] - floor) / EROSION_STRENGTH;
            if (drop > seepage && grid->cells[r][c] > BEDROCK_LEVEL) {
                grid->cells[r][c] -= drop;
                eroded_cells += 1;
            }
        }
    grid->steps += 1;
}

static int grid_total(const struct Grid *grid) {
    int sum = 0;
    for (int r = 0; r < GRID_ROWS; ++r)
        for (int c = 0; c < GRID_COLS; ++c)
            sum += grid->cells[r][c];
    return sum;
}

int grid_erosion_check(void) {
    struct Grid grid = {
        { {8, 7, 6, 5, 4}, {6, 9, 8, 7, 3}, {4, 5, 6, 9, 2}, {3, 2, 1, 2, 5} }, 0
    };
    int before = grid_total(&grid);
    erode_step(&grid);
    erode_step(&grid);
    int after = grid_total(&grid);
    if (grid.steps != 2) return 1;
    if (eroded_cells <= 0) return 2;
    if (after >= before) return 3;
    if (after <= 0) return 4;
    return 0;
}
