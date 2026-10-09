#define MATRIX_ROWS 4
#define MATRIX_COLS 3
#define MATRIX_TAG 100
#define FEATURE_TURBO 1
#define FEATURE_SAFE 2
#ifndef MATRIX_ENGINE
#define MATRIX_ENGINE 0
#endif
#define SCALE_FACTOR 3
#if FEATURE_TURBO == 1 && defined(FEATURE_TURBO)
#define ENGINE_MODE 2
#elif defined(FEATURE_SAFE)
#define ENGINE_MODE 1
#else
#define ENGINE_MODE 0
#endif
#define IDX(row, col) ((row) * MATRIX_COLS + (col))
#define CELL_SUM(a, b) ((a) + (b))
#define WEIGHTED(x) \
    ((x) * SCALE_FACTOR)
#define BUMP(p, n) do { (p)[0] = (p)[0] + (n); } while (0)

static int matrix_total(const int *grid) {
    int total = 0;
    for (int i = 0; i < MATRIX_ROWS * MATRIX_COLS; i++) {
        total = CELL_SUM(total, grid[i]);
    }
    return total;
}

int macro_config_matrix_check(void) {
    int grid[MATRIX_ROWS * MATRIX_COLS] = { 0 };
    int *rows = grid;
    int expected = 0;
    for (int r = 0; r < MATRIX_ROWS; r++) {
        for (int c = 0; c < MATRIX_COLS; c++) {
            grid[IDX(r, c)] = r + c + MATRIX_TAG;
        }
    }
    if (ENGINE_MODE != 2) {
        return 1;
    }
    BUMP(rows, WEIGHTED(ENGINE_MODE));
    if (rows[0] != MATRIX_TAG + WEIGHTED(ENGINE_MODE)) {
        return 2;
    }
    for (int r = 0; r < MATRIX_ROWS; r++) {
        for (int c = 0; c < MATRIX_COLS; c++) {
            expected = CELL_SUM(expected, IDX(r, c) - IDX(r, c) + r + c + MATRIX_TAG);
        }
    }
    if (matrix_total(grid) != expected + WEIGHTED(ENGINE_MODE)) {
        return 3;
    }
    return 0;
}
