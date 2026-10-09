static int attempt_backoff(int attempt) {
    int delay = 1, step = 0;
    do {
        delay *= 2;
        step++;
    } while (step < attempt && delay < 64);
    return delay;
}

static int cap_rounds(int seed, int cap) {
    int total = 0;
    for (int r = 0; r < cap; r++) {
        if (r % 2 == 0) continue;
        total += seed + r;
        if (total > 100) break;
    }
    return total;
}

static int spend_budget(int operations, int budget) {
    int used = 0, round = 0;
    while (used < budget) {
        round++;
        int cost = 0, op = 0;
        while (op < operations) {
            if (used + cost > budget) break;
            cost += 1 + (op % 3);
            op++;
        }
        if (cost == 0) return -used;
        used += cost;
        if (round > 50) return -1000;
    }
    return used;
}

int retry_budget_check(void) {
    if (attempt_backoff(0) != 2) return 1;
    if (attempt_backoff(3) != 8) return 2;
    if (attempt_backoff(100) != 64) return 3;
    if (cap_rounds(10, 5) != 24) return 4;
    if (cap_rounds(10, 100) != 119) return 5;
    if (spend_budget(4, 9) != 10) return 6;
    if (spend_budget(0, 5) != 0) return 7;
    if (spend_budget(1, 1000) != -1000) return 8;
    return 0;
}
