#define FLAG_READ 1
#define FLAG_WRITE 2
#define FLAG_AUDIT 4
#define ENTRY_COUNT 6

const int required_mask_10 = FLAG_READ | FLAG_AUDIT;
static int rejected_10 = 0;

static int apply_flags_10(int current, int extra) {
    int merged = current | extra;
    if ((merged & FLAG_WRITE) != 0) {
        merged ^= FLAG_WRITE;
        merged |= FLAG_AUDIT;
    }
    return merged;
}

static int validate_flags_10(const int *entries) {
    int accepted = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i) {
        int merged = apply_flags_10(entries[i], FLAG_AUDIT);
        if ((merged & required_mask_10) == required_mask_10) {
            accepted += 1;
        } else {
            rejected_10 += 1;
        }
    }
    return accepted;
}

int main(void) {
    int entries[ENTRY_COUNT] = {FLAG_READ, FLAG_WRITE, 0, FLAG_AUDIT, FLAG_READ | FLAG_WRITE, FLAG_READ | FLAG_AUDIT};
    int accepted = validate_flags_10(entries);
    if (accepted < 3) return 1;
    if (rejected_10 <= 0) return 2;
    if (apply_flags_10(FLAG_READ, 0) != FLAG_READ) return 3;
    return 0;
}
