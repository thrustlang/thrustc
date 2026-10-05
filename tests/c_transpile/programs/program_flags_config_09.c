#define FLAG_READ 1
#define FLAG_WRITE 2
#define FLAG_AUDIT 4
#define ENTRY_COUNT 7

const int required_mask_09 = FLAG_READ;
static int rejected_09 = 0;

static int apply_flags_09(int current, int extra) {
    int merged = current | extra;
    if ((merged & FLAG_WRITE) != 0) {
        merged ^= FLAG_WRITE;
        merged |= FLAG_AUDIT;
    }
    return merged;
}

static int validate_flags_09(const int *entries) {
    int accepted = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i) {
        int merged = apply_flags_09(entries[i], FLAG_READ);
        if ((merged & required_mask_09) == required_mask_09) {
            accepted += 1;
        } else {
            rejected_09 += 1;
        }
    }
    return accepted;
}

int main(void) {
    int entries[ENTRY_COUNT] = {FLAG_WRITE, FLAG_AUDIT, 0, FLAG_READ, FLAG_READ | FLAG_WRITE, FLAG_AUDIT | FLAG_WRITE, FLAG_READ | FLAG_AUDIT};
    int accepted = validate_flags_09(entries);
    if (accepted < 5) return 1;
    if (rejected_09 < 0) return 2;
    if ((apply_flags_09(FLAG_WRITE, 0) & FLAG_WRITE) != 0) return 3;
    return 0;
}
