#define POINTER_4(out) do { \
    int value = 7; \
    int *cursor = &value; \
    int index; \
    for (index = 0; index < 6; index++) { \
        *cursor += 3; \
    } \
    (out) = value; \
} while (0)

int main(void) {
    int out = 0;
    POINTER_4(out);
    return out == 25 ? 0 : 1;
}
