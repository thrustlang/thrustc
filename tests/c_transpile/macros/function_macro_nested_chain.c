#define ONE() (1)
#define ADD1(x) ((x) + ONE())

int function_macro_nested_chain(int x) {
    return ADD1(x);
}
