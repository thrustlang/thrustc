typedef float v8f __attribute__((vector_size(32)));

struct Inner {
    v8f value;
};

struct Outer {
    struct Inner inner;
    int tag;
};

struct Outer identity(struct Outer value) {
    return value;
}
