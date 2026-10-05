#define FLAG_READ 1
#define FLAG_WRITE 2
#define FLAG_AUDIT 4
#define ENTRY_COUNT 6

const int required_mask_04 = FLAG_READ;
static int rejected_04 = 0;

static int apply_flags_04(int current, int extra) {
    int merged = current | extra;
    if ((merged & FLAG_WRITE) != 0) {
        merged ^= FLAG_WRITE;
        merged |= FLAG_AUDIT;
    }
    return merged;
}

static int validate_flags_04(const int *entries) {
    int accepted = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i) {
        int merged = apply_flags_04(entries[i], FLAG_READ);
        if ((merged & required_mask_04) == required_mask_04) {
            accepted += 1;
        } else {
            rejected_04 += 1;
        }
    }
    return accepted;
}

int main(void) {
    int entries[ENTRY_COUNT] = {FLAG_WRITE, 0, FLAG_AUDIT, FLAG_READ, FLAG_READ | FLAG_WRITE, FLAG_AUDIT | FLAG_WRITE};
    int accepted = validate_flags_04(entries);

    if (accepted < 4) {
        return 1;
    }
    if (rejected_04 < 0) {
        return 2;
    }
    if ((apply_flags_04(FLAG_WRITE, FLAG_READ) & FLAG_WRITE) != 0) {
        return 3;
    }
    return 0;
}
