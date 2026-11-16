typedef float v16f __attribute__((vector_size(64)));

struct V {
    v16f value;
};

struct V identity(struct V value) {
    return value;
}
