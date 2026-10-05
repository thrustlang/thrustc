typedef struct {
    int slots[4];
} SmallSet;

#define MY_ZERO(s) do { unsigned int i; SmallSet *arr = (s); for (i = 0; i < sizeof(SmallSet) / sizeof(int); ++i) arr->slots[i] = 0; } while (0)

int statement_do_while(void) {
    SmallSet set;
    set.slots[0] = 7;
    set.slots[3] = 9;
    MY_ZERO(&set);
    return set.slots[0] + set.slots[3];
}
