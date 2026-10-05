#define FLAG_READ 1
#define FLAG_WRITE 2
#define FLAG_AUDIT 4
#define ENTRY_COUNT 6

const int required_mask_07 = FLAG_READ | FLAG_AUDIT;
static int rejected_07 = 0;

static int apply_flags_07(int current, int extra) {
    int merged = current | extra;
    if ((merged & FLAG_WRITE) != 0) {
        merged ^= FLAG_WRITE;
        merged |= FLAG_AUDIT;
    }
    return merged;
}

static int validate_flags_07(const int *entries) {
    int accepted = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i) {
        int merged = apply_flags_07(entries[i], FLAG_AUDIT);
        if ((merged & required_mask_07) == required_mask_07) {
            accepted += 1;
        } else {
            rejected_07 += 1;
        }
    }
    return accepted;
}

int main(void) {
    int entries[ENTRY_COUNT] = {FLAG_READ, 0, FLAG_WRITE, FLAG_AUDIT, FLAG_READ | FLAG_WRITE, FLAG_READ | FLAG_AUDIT};
    int accepted = validate_flags_07(entries);
    if (accepted < 3) return 1;
    if (rejected_07 <= 0) return 2;
    if ((apply_flags_07(FLAG_WRITE, 0) & FLAG_AUDIT) == 0) return 3;
    return 0;
}
