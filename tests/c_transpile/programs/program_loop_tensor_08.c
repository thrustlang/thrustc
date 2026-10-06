static int loop_hits_08 = 0;

static int walk_tensor_08(int *flat) {
    int total = 0;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 4; ++col) {
            int k = row * 4 + col;
            int v = flat[k];
            if ((v % 2) == 0) total += v + row;
            else total -= v - col;
            loop_hits_08 += 1;
        }
    }
    return total;
}

int main(void) {
    int data[12] = {-2, 6, 2, -1, 11, -4, -1, -2, -7, 4, -2, 5};
    int total = walk_tensor_08(data);
    if (loop_hits_08 != 12) return 1;
    if (total != 9) return 2;
    return 0;
}
