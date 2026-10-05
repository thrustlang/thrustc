#define ROWS 4
#define COLS 6
#define BAND_LIMIT 7

const int matrix_bias_03 = 1;
static int visited_cells_03 = 0;

static void fill_matrix_03(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 3) * (c + 1) + matrix_bias_03;
        }
    }
}

static int trace_band_03(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_03 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_03(matrix);
    int total = trace_band_03(matrix);

    if (visited_cells_03 != ROWS * COLS) {
        return 1;
    }
    if (total <= 50) {
        return 2;
    }
    if (matrix[2][2] <= matrix_bias_03) {
        return 3;
    }
    return 0;
}
