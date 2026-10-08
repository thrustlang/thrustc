#define LEVEL 2
#if LEVEL == 2
#define PICKED 20
#else
#define PICKED 10
#endif
int picked_val(void) {
    return PICKED;
}
