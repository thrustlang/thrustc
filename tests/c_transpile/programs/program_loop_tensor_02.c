static int loop_hits_02 = 0;

static int walk_tensor_02(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_02 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {-4, -6, 7, 11, 10, -9, 11, 1, 0, -9, 1, 2};
    int total = walk_tensor_02(data);
    if (loop_hits_02 != 12) return 1;
    if (total != 8) return 2;
    return 0;
}
