struct Node {
    int key;
    int sum;
};

static void fold_node(struct Node *node, int extra) {
    node->sum = node->key + extra;
}

static int find_key_index(struct Node *nodes, int n, int key) {
    for (int i = 0; i < n; i++) {
        if (nodes[i].key == key) {
            return i;
        }
    }
    return 0;
}

static int sum_nodes(struct Node *nodes, int n) {
    int total = 0;
    for (int i = 0; i < n; i++) {
        total += nodes[i].sum;
    }
    return total;
}

int node_chain_check(void) {
    struct Node nodes[5] = {
        {1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}
    };
    struct Node *head = nodes;
    struct Node **slot = &head;

    if ((*slot)->key != 1) { return 1; }

    for (int i = 0; i < 5; i++) {
        fold_node(head + i, i * 10);
    }
    if (nodes[0].sum != 1) { return 2; }
    if (nodes[4].sum != 45) { return 3; }
    if (sum_nodes(nodes, 5) != 115) { return 4; }

    int found = find_key_index(nodes, 5, 3);
    if (nodes[found].key != 3) { return 5; }
    nodes[found].sum = nodes[found].sum + 100;
    if (nodes[2].sum != 123) { return 6; }

    int tail = nodes[4].key;
    tail = tail + nodes[3].key;
    if (tail != 9) { return 7; }

    return 0;
}
