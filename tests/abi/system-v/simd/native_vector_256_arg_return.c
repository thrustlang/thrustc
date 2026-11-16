typedef float v8f __attribute__((vector_size(32)));

v8f add_vectors(v8f left, v8f right) {
    return left + right;
}
