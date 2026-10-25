#define POINTER_1(out) do { \
    int value = 1; \
    int *cursor = &value; \
    int index; \
    for (index = 0; index < 4; index++) { \
        *cursor += 1; \
    } \
    (out) = value; \
} while (0)

int main(void) {
    int out = 0;
    POINTER_1(out);
    return out == 5 ? 0 : 1;
}
