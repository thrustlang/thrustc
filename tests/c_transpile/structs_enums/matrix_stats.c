typedef enum {
    OP_SUM = 0,
    OP_MIN = 1,
    OP_MAX = 2,
} ReduceOp;
typedef struct {
    int rows;
    int cols;
    int cells[4][4];
} Matrix;

static int matrix_get(Matrix m, int r, int c) {
    if (r < 0 || c < 0 || r >= m.rows || c >= m.cols) return -1;
    return m.cells[r][c];
}
static void matrix_fill(Matrix *m, int value) {
    for (int r = 0; r < m->rows; r++) {
        for (int c = 0; c < m->cols; c++) {
            m->cells[r][c] = value + r * m->cols + c;
        }
    }
}
static int matrix_reduce(const Matrix *m, ReduceOp op) {
    int acc = (op == OP_SUM) ? 0 : m->cells[0][0];
    for (int r = 0; r < m->rows; r++) {
        for (int c = 0; c < m->cols; c++) {
            int v = m->cells[r][c];
            switch (op) {
                case OP_SUM: acc += v; break;
                case OP_MIN: if (v < acc) acc = v; break;
                case OP_MAX: if (v > acc) acc = v; break;
                default: break;
            }
        }
    }
    return acc;
}
static Matrix matrix_transpose(Matrix m) {
    Matrix out = m;
    for (int r = 0; r < m.rows; r++) {
        for (int c = 0; c < m.cols; c++) {
            out.cells[c][r] = m.cells[r][c];
        }
    }
    return out;
}
int matrix_stats_check(void) {
    Matrix m = { .rows = 2, .cols = 3, .cells = {{0}} };
    matrix_fill(&m, 1);
    if (matrix_get(m, 0, 0) != 1) return 1;
    if (matrix_get(m, 1, 2) != 6) return 2;
    if (matrix_get(m, 2, 0) != -1) return 3;
    if (matrix_reduce(&m, OP_SUM) != 1 + 2 + 3 + 4 + 5 + 6) return 4;
    if (matrix_reduce(&m, OP_MAX) != 6) return 5;
    if (matrix_reduce(&m, OP_MIN) != 1) return 6;
    Matrix t = matrix_transpose(m);
    if (t.rows != 2 || t.cols != 3) return 7;
    if (t.cells[0][1] != m.cells[1][0]) return 8;
    return 0;
}
