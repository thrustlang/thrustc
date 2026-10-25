#define WRITE_INDEX(base, index, value) do { \
    (base)[index] = (value); \
} while (0)

int main(void) {
    int flat[4] = {0, 0, 0, 0};
    int *pointer = flat;

    WRITE_INDEX(flat, 2, 11);
    WRITE_INDEX(pointer, 1, 12);

    if (flat[1] != 12) return 1;
    if (flat[2] != 11) return 2;
    return 0;
}
