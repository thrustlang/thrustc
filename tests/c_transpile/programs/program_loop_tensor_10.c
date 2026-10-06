static int loop_hits_10 = 0;

static int walk_tensor_10(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_10 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {1, 11, -3, -3, -7, -3, -5, 4, 11, -3, -4, 7};
    int total = walk_tensor_10(data);
    if (loop_hits_10 != 12) return 1;
    if (total != 10) return 2;
    return 0;
}
