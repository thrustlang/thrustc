#define FLAG_READ 1
#define FLAG_WRITE 2
#define FLAG_AUDIT 4
#define ENTRY_COUNT 7

const int required_mask_03 = FLAG_AUDIT;
static int rejected_03 = 0;

static int apply_flags_03(int current, int extra) {
    int merged = current | extra;
    if ((merged & FLAG_WRITE) != 0) {
        merged ^= FLAG_WRITE;
        merged |= FLAG_AUDIT;
    }
    return merged;
}

static int validate_flags_03(const int *entries) {
    int accepted = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i) {
        int merged = apply_flags_03(entries[i], 0);
        if ((merged & required_mask_03) == required_mask_03) {
            accepted += 1;
        } else {
            rejected_03 += 1;
        }
    }
    return accepted;
}

int main(void) {
    int entries[ENTRY_COUNT] = {FLAG_WRITE, FLAG_READ, FLAG_AUDIT, FLAG_READ | FLAG_WRITE, 0, FLAG_AUDIT, FLAG_WRITE};
    int accepted = validate_flags_03(entries);

    if (accepted < 4) {
        return 1;
    }
    if (rejected_03 <= 0) {
        return 2;
    }
    if (apply_flags_03(FLAG_READ, FLAG_AUDIT) != (FLAG_READ | FLAG_AUDIT)) {
        return 3;
    }
    return 0;
}
