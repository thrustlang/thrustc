typedef float v16f __attribute__((vector_size(64)));

v16f add_vectors(v16f left, v16f right) {
    return left + right;
}
