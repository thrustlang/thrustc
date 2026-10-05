#define ROWS 4
#define COLS 5
#define BAND_LIMIT 7

const int matrix_bias_10 = 6;
static int visited_cells_10 = 0;

static void fill_matrix_10(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 1) * (c + 5) + matrix_bias_10;
        }
    }
}

static int trace_band_10(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_10 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_10(matrix);
    int total = trace_band_10(matrix);

    if (visited_cells_10 != ROWS * COLS) {
        return 1;
    }
    if (total <= 80) {
        return 2;
    }
    if (matrix[3][4] <= matrix_bias_10) {
        return 3;
    }
    return 0;
}
