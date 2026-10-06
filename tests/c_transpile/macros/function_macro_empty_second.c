#define SECOND(a, b) (b)

int function_macro_empty_second(void) {
    return SECOND(, 2);
}
