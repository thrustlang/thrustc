#define ROWS 5
#define COLS 5
#define BAND_LIMIT 8

const int matrix_bias_05 = 2;
static int visited_cells_05 = 0;

static void fill_matrix_05(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 2) * (c + 2) + matrix_bias_05;
        }
    }
}

static int trace_band_05(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_05 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_05(matrix);
    int total = trace_band_05(matrix);

    if (visited_cells_05 != ROWS * COLS) {
        return 1;
    }
    if (total <= 70) {
        return 2;
    }
    if (matrix[4][4] <= matrix_bias_05) {
        return 3;
    }
    return 0;
}
