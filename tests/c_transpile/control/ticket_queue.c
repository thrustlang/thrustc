static int classify(int ticket) {
    int lane = 0;
    switch (ticket % 5) {
        case 0:
            lane = 3;
            break;
        case 1:
        case 2:
            lane = 2;
            /* fall through */
        case 3:
            lane += 1;
            break;
        default:
            lane = -1;
            break;
    }
    return lane;
}

static int serve_queue(const int *tickets, int n, int capacity) {
    int served = 0, index = 0;
    while (index < n) {
        int t = tickets[index];
        index++;
        if (t < 0) continue;
        int lane = classify(t);
        if (lane < 0) continue;
        do {
            if (served >= capacity) return served;
            served++;
            lane--;
        } while (lane > 0);
    }
    return served;
}

int ticket_queue_check(void) {
    int tickets[7] = {0, 1, 2, 3, 4, -1, 6};
    if (serve_queue(tickets, 7, 100) != 13) return 1;
    int pair[2] = {0, 0};
    if (serve_queue(pair, 2, 4) != 4) return 2;
    if (classify(4) != -1 || classify(1) != 3 || classify(3) != 1) return 3;
    if (serve_queue(tickets, 0, 10) != 0) return 4;
    return 0;
}
