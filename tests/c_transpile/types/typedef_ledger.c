#include <stdbool.h>

typedef unsigned int u32;
typedef short score_t;
typedef score_t score_row_t[6];
typedef long long total_t;
typedef int (*binop_t)(int, int);

typedef struct ledger_entry {
    score_t weight;
    u32 count;
    total_t running;
} ledger_entry;

static score_t clamp_score(score_t s, score_t lo, score_t hi) {
    if (s < lo) return lo;
    if (s > hi) return hi;
    return s;
}

static int add_scores(int a, int b) { return a + b; }
static int max_scores(int a, int b) { return a > b ? a : b; }

static total_t sum_row(const score_row_t row, int len, binop_t combine) {
    total_t acc = 0;
    for (int i = 0; i < len; i = i + 1) {
        acc = (total_t)combine((int)acc, (int)row[i]);
    }
    return acc;
}

static bool entry_valid(const ledger_entry *e, u32 floor) {
    return e->count >= floor && e->weight >= 0;
}

int typedef_ledger_check(void) {
    score_row_t row = {4, 9, 2, 7, 5, 1};
    if (sum_row(row, 6, add_scores) != 28) return 1;
    if (sum_row(row, 6, max_scores) != 9) return 2;

    ledger_entry e;
    e.weight = clamp_score(12, 0, 10);
    e.count = 3u;
    e.running = sum_row(row, 6, add_scores);
    if (e.weight != 10) return 3;
    if (!entry_valid(&e, 1u)) return 4;
    if (e.running != 28) return 5;

    score_row_t other = {1, 2, 3, 4, 5, 6};
    int diff = 0;
    for (u32 i = 0; i < 6u; i = i + 1) {
        diff = diff + (int)(other[i] - row[i]);
    }
    if (diff != -7) return 6;

    return 0;
}
