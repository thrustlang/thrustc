typedef float v8f __attribute__((vector_size(32)));

struct V {
    v8f left;
    v8f right;
};

struct V identity(struct V value) {
    return value;
}
