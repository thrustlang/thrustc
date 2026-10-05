#define ROWS 4
#define COLS 5
#define BAND_LIMIT 10

const int matrix_bias_01 = 2;
static int visited_cells_01 = 0;

static void fill_matrix_01(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 1) * (c + 2) + matrix_bias_01;
        }
    }
}

static int trace_band_01(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_01 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_01(matrix);
    int total = trace_band_01(matrix);

    if (visited_cells_01 != ROWS * COLS) {
        return 1;
    }
    if (total <= 40) {
        return 2;
    }
    if (matrix[0][0] <= matrix_bias_01) {
        return 3;
    }
    return 0;
}
