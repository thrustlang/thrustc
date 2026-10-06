static int loop_hits_05 = 0;

static int walk_tensor_05(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_05 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {-3, 0, 4, 6, -2, 10, -3, 8, 3, 8, 4, -7};
    int total = walk_tensor_05(data);
    if (loop_hits_05 != 12) return 1;
    if (total != 60) return 2;
    return 0;
}
