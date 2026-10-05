#define ROWS 5
#define COLS 4
#define BAND_LIMIT 6

const int matrix_bias_02 = 3;
static int visited_cells_02 = 0;

static void fill_matrix_02(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 2) * (c + 1) + matrix_bias_02;
        }
    }
}

static int trace_band_02(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_02 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_02(matrix);
    int total = trace_band_02(matrix);

    if (visited_cells_02 != ROWS * COLS) {
        return 1;
    }
    if (total <= 45) {
        return 2;
    }
    if (matrix[1][1] <= matrix_bias_02) {
        return 3;
    }
    return 0;
}
