#define POINTER_3(out) do { \
    int value = 10; \
    int *cursor = &value; \
    int index; \
    for (index = 0; index < 3; index++) { \
        *cursor += 5; \
    } \
    (out) = value; \
} while (0)

int main(void) {
    int out = 0;
    POINTER_3(out);
    return out == 25 ? 0 : 1;
}
