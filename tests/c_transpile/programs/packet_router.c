#define PACKET_COUNT 7
#define ROUTE_COUNT 4
#define TTL_FLOOR 3
#define HOP_COST 2

struct Packet {
    int src;
    int dst;
    int ttl;
    int flags;
};

struct Route {
    int target;
    int next_hop;
};

const int default_hop = 9;
static int delivered = 0;
static int dropped = 0;

static int lookup_hop(const struct Route *table, int target) {
    for (int i = 0; i < ROUTE_COUNT; ++i)
        if (table[i].target == target) return table[i].next_hop;
    return default_hop;
}

static int forward(struct Packet *packet, const struct Route *table) {
    int hop = lookup_hop(table, packet->dst);
    if (hop == packet->src) { dropped += 1; return -1; }
    packet->ttl -= HOP_COST;
    packet->flags |= 1;
    if (packet->ttl < TTL_FLOOR) { dropped += 1; return -1; }
    delivered += 1;
    return hop;
}

int packet_router_check(void) {
    const struct Route table[ROUTE_COUNT] = {
        {1, 2}, {2, 3}, {3, 1}, {4, 0}
    };
    struct Packet packets[PACKET_COUNT] = {
        {0, 1, 8, 0}, {1, 3, 6, 0}, {2, 2, 4, 0}, {3, 4, 9, 0},
        {4, 1, 2, 0}, {5, 4, 7, 0}, {6, 3, 5, 0}
    };
    int total_ttl = 0;
    for (int i = 0; i < PACKET_COUNT; ++i)
        if (forward(&packets[i], table) >= 0) total_ttl += packets[i].ttl;
    if (delivered + dropped != PACKET_COUNT) return 1;
    if (delivered <= 0) return 2;
    if (total_ttl <= TTL_FLOOR) return 3;
    return 0;
}
