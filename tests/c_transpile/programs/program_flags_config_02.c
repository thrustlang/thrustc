#define FLAG_READ 1
#define FLAG_WRITE 2
#define FLAG_AUDIT 4
#define ENTRY_COUNT 5

const int required_mask_02 = FLAG_READ | FLAG_AUDIT;
static int rejected_02 = 0;

static int apply_flags_02(int current, int extra) {
    int merged = current | extra;
    if ((merged & FLAG_WRITE) != 0) {
        merged ^= FLAG_WRITE;
        merged |= FLAG_AUDIT;
    }
    return merged;
}

static int validate_flags_02(const int *entries) {
    int accepted = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i) {
        int merged = apply_flags_02(entries[i], FLAG_READ);
        if ((merged & required_mask_02) == required_mask_02) {
            accepted += 1;
        } else {
            rejected_02 += 1;
        }
    }
    return accepted;
}

int main(void) {
    int entries[ENTRY_COUNT] = {0, FLAG_WRITE, FLAG_AUDIT, FLAG_READ, FLAG_READ | FLAG_WRITE};
    int accepted = validate_flags_02(entries);

    if (accepted < 2) {
        return 1;
    }
    if (rejected_02 <= 0) {
        return 2;
    }
    if ((apply_flags_02(FLAG_WRITE, 0) & FLAG_AUDIT) == 0) {
        return 3;
    }
    return 0;
}
