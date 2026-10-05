#define ROWS 6
#define COLS 3
#define BAND_LIMIT 6

const int matrix_bias_04 = 4;
static int visited_cells_04 = 0;

static void fill_matrix_04(int matrix[ROWS][COLS]) {
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            matrix[r][c] = (r + 1) * (c + 3) + matrix_bias_04;
        }
    }
}

static int trace_band_04(int matrix[ROWS][COLS]) {
    int total = 0;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            if ((r + c) <= BAND_LIMIT) {
                total += matrix[r][c];
            }
            visited_cells_04 += 1;
        }
    }
    return total;
}

int main(void) {
    int matrix[ROWS][COLS];
    fill_matrix_04(matrix);
    int total = trace_band_04(matrix);

    if (visited_cells_04 != ROWS * COLS) {
        return 1;
    }
    if (total <= 42) {
        return 2;
    }
    if (matrix[3][1] <= matrix_bias_04) {
        return 3;
    }
    return 0;
}
