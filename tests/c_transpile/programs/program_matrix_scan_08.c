#define ROWS 3
#define COLS 7
#define BAND_LIMIT 6

const int matrix_bias_08 = 4;
static int visited_cells_08 = 0;

static void fill_matrix_08(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 4) * (c + 1) + matrix_bias_08;
        }
    }
}

static int trace_band_08(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_08 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_08(matrix);
    int total = trace_band_08(matrix);

    if (visited_cells_08 != ROWS * COLS) {
        return 1;
    }
    if (total <= 65) {
        return 2;
    }
    if (matrix[2][6] <= matrix_bias_08) {
        return 3;
    }
    return 0;
}
