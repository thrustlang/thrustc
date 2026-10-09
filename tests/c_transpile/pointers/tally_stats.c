struct Sample {
    int value;
    int weight;
};

static void bump(struct Sample *s, int amount) {
    s->value = s->value + amount;
    s->weight++;
}

static int weighted_total(struct Sample *items, int n) {
    int total = 0;
    struct Sample *p = items;
    struct Sample *end = items + n;
    while (p < end) {
        total = total + p->value * p->weight;
        p++;
    }
    return total;
}

int tally_stats_check(void) {
    struct Sample items[4] = {
        {2, 1}, {3, 1}, {4, 1}, {5, 1}
    };
    struct Sample *first = items;
    struct Sample *third = first + 2;

    if (first->value != 2) { return 1; }
    if (third->value != 4) { return 2; }

    bump(first, 5);
    if (first->value != 7) { return 3; }
    if (first->weight != 2) { return 4; }

    struct Sample *cursor = items;
    for (int i = 0; i < 4; i++, cursor++) {
        bump(cursor, i);
    }
    if (items[0].value != 7) { return 5; }
    if (items[3].value != 8) { return 6; }
    if (weighted_total(items, 4) != 57) { return 7; }

    return 0;
}
