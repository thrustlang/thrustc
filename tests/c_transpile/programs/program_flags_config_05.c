#define FLAG_READ 1
#define FLAG_WRITE 2
#define FLAG_AUDIT 4
#define ENTRY_COUNT 5

const int required_mask_05 = FLAG_READ | FLAG_AUDIT;
static int rejected_05 = 0;

static int apply_flags_05(int current, int extra) {
    int merged = current | extra;
    if ((merged & FLAG_WRITE) != 0) {
        merged ^= FLAG_WRITE;
        merged |= FLAG_AUDIT;
    }
    return merged;
}

static int validate_flags_05(const int *entries) {
    int accepted = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i) {
        int merged = apply_flags_05(entries[i], FLAG_READ);
        if ((merged & required_mask_05) == required_mask_05) {
            accepted += 1;
        } else {
            rejected_05 += 1;
        }
    }
    return accepted;
}

int main(void) {
    int entries[ENTRY_COUNT] = {FLAG_AUDIT, FLAG_WRITE, FLAG_READ, FLAG_READ | FLAG_AUDIT, 0};
    int accepted = validate_flags_05(entries);

    if (accepted < 3) {
        return 1;
    }
    if (rejected_05 <= 0) {
        return 2;
    }
    if ((apply_flags_05(0, FLAG_READ) & FLAG_READ) == 0) {
        return 3;
    }
    return 0;
}
