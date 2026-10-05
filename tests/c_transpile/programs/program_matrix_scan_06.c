#define ROWS 4
#define COLS 4
#define BAND_LIMIT 5

const int matrix_bias_06 = 5;
static int visited_cells_06 = 0;

static void fill_matrix_06(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 1) * (c + 4) + matrix_bias_06;
        }
    }
}

static int trace_band_06(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_06 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_06(matrix);
    int total = trace_band_06(matrix);

    if (visited_cells_06 != ROWS * COLS) {
        return 1;
    }
    if (total <= 55) {
        return 2;
    }
    if (matrix[0][3] <= matrix_bias_06) {
        return 3;
    }
    return 0;
}
