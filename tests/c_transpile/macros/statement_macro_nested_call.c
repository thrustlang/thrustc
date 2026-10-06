#define INC(x) ((x) + 1)
#define ASSIGN_INC(dst, src) do { (dst) = INC(src); } while (0)

int statement_macro_nested_call(void) {
    int value = 0;
    ASSIGN_INC(value, 41);
    return value;
}
