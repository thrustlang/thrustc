static int route_cost(int code, int weight) {
    int cost = 0;
    switch (code) {
        case 1:
            cost += weight * 2;
            /* fall through */
        case 2:
            cost += weight;
            break;
        case 3:
        case 4:
            cost += weight / 2 + 1;
            break;
        case 5:
            cost += weight * 3;
            break;
        default:
            cost = weight + 7;
            break;
    }
    return cost;
}

static int route_batch(const int *codes, const int *weights, int n) {
    int total = 0;
    for (int i = 0; i < n; i++) {
        if (codes[i] < 0) continue;
        if (codes[i] > 5) return -1;
        int cost = route_cost(codes[i], weights[i]);
        for (int j = 0; j < codes[i]; j++) {
            if (j == 2) break;
            cost += j;
        }
        total += cost;
    }
    return total;
}

int packet_route_check(void) {
    int codes[6] = {1, 2, 3, 4, 5, 0};
    int weights[6] = {4, 3, 2, 6, 1, 5};
    if (route_batch(codes, weights, 6) != 40) return 1;
    int bad_codes[3] = {-1, 6, 2};
    int bad_weights[3] = {7, 7, 7};
    if (route_batch(bad_codes, bad_weights, 3) != -1) return 2;
    if (route_batch(codes, weights, 0) != 0) return 3;
    return 0;
}
