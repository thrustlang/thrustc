static int loop_hits_09 = 0;

static int walk_tensor_09(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_09 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {9, 8, -3, 0, -4, -5, 12, 7, 10, 12, 11, 4};
    int total = walk_tensor_09(data);
    if (loop_hits_09 != 12) return 1;
    if (total != 39) return 2;
    return 0;
}
