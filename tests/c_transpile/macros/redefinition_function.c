#define ADD(a, b) ((a) + (b))
#define ADD(a, b) ((a) - (b))

int redefinition_function(int x, int y) {
    return ADD(x, y);
}
