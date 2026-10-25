#if 1
#define ACTIVE_VALUE 12
#else
#if 1
#define BAD(x) do { goto x; } while (0)
#else
#define BAD(x) do { x = 0; } while (0)
#endif
#endif

int ext_cond_nested_05_value(void) {
    return ACTIVE_VALUE;
}
