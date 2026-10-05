#define ROWS 5
#define COLS 6
#define BAND_LIMIT 8

const int matrix_bias_09 = 1;
static int visited_cells_09 = 0;

static void fill_matrix_09(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 3) * (c + 2) + matrix_bias_09;
        }
    }
}

static int trace_band_09(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_09 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_09(matrix);
    int total = trace_band_09(matrix);

    if (visited_cells_09 != ROWS * COLS) {
        return 1;
    }
    if (total <= 100) {
        return 2;
    }
    if (matrix[4][5] <= matrix_bias_09) {
        return 3;
    }
    return 0;
}
