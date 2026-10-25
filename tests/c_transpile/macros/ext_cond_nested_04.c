#define OUTER 1
#define ENABLE_BASE 1

#if OUTER
#if ENABLE_BASE
#define PICK_VALUE(x) ((x) + 4)
#define BASE_VALUE 60
#else
#define PICK_VALUE(x) ((x) - 4)
#define BASE_VALUE 0
#endif
#else
#define PICK_VALUE(x) ((x) * 2)
#define BASE_VALUE 1
#endif

int ext_cond_nested_04_value(void) {
    return PICK_VALUE(BASE_VALUE);
}
