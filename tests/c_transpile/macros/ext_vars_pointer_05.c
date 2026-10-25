#define POINTER_5(out) do { \
    int value = 20; \
    int *cursor = &value; \
    int index; \
    for (index = 0; index < 4; index++) { \
        *cursor += 4; \
    } \
    (out) = value; \
} while (0)

int main(void) {
    int out = 0;
    POINTER_5(out);
    return out == 36 ? 0 : 1;
}
