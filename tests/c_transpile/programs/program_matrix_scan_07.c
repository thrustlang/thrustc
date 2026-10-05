#define ROWS 6
#define COLS 4
#define BAND_LIMIT 7

const int matrix_bias_07 = 3;
static int visited_cells_07 = 0;

static void fill_matrix_07(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 2) * (c + 3) + matrix_bias_07;
        }
    }
}

static int trace_band_07(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_07 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_07(matrix);
    int total = trace_band_07(matrix);

    if (visited_cells_07 != ROWS * COLS) {
        return 1;
    }
    if (total <= 85) {
        return 2;
    }
    if (matrix[5][0] <= matrix_bias_07) {
        return 3;
    }
    return 0;
}
