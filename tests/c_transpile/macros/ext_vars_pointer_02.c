#define POINTER_2(out) do { \
    int value = 3; \
    int *cursor = &value; \
    int index; \
    for (index = 0; index < 5; index++) { \
        *cursor += 2; \
    } \
    (out) = value; \
} while (0)

int main(void) {
    int out = 0;
    POINTER_2(out);
    return out == 13 ? 0 : 1;
}
