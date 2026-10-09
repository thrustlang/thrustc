typedef enum {
    CAT_TOOL = 1,
    CAT_FOOD = 3,
    CAT_MISC = 7,
} Category;
typedef struct {
    int id;
    Category cat;
    int qty;
    int price;
} Item;
typedef struct {
    Item items[8];
    int count;
} Inventory;

static int item_value(Item it) {
    return it.qty * it.price;
}
static int category_weight(Category c) {
    switch (c) {
        case CAT_TOOL: return 4;
        case CAT_FOOD: return 2;
        case CAT_MISC: return 1;
        default:       return 0;
    }
}
static void inventory_add(Inventory *inv, int id, Category cat, int qty, int price) {
    if (inv->count >= 8) return;
    Item it = { id, cat, qty, price };
    inv->items[inv->count] = it;
    inv->count++;
}
static int inventory_total(const Inventory *inv) {
    int total = 0;
    for (int i = 0; i < inv->count; i++) {
        total += item_value(inv->items[i]);
    }
    return total;
}
static int inventory_weight(const Inventory *inv) {
    int weight = 0;
    for (int i = 0; i < inv->count; i++) {
        weight += category_weight(inv->items[i].cat) * inv->items[i].qty;
    }
    return weight;
}

int inventory_totals_check(void) {
    Inventory inv = { .count = 0 };
    inventory_add(&inv, 1, CAT_TOOL, 2, 10);
    inventory_add(&inv, 2, CAT_FOOD, 5, 3);
    inventory_add(&inv, 3, CAT_MISC, 1, 7);
    inventory_add(&inv, 4, CAT_TOOL, 1, 25);
    if (inv.count != 4) return 1;
    if (inventory_total(&inv) != 2 * 10 + 5 * 3 + 1 * 7 + 1 * 25) return 2;
    if (inventory_weight(&inv) != 4 * 2 + 2 * 5 + 1 * 1 + 4 * 1) return 3;
    if (item_value(inv.items[3]) != 25) return 4;
    return 0;
}
