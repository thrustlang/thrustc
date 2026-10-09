#define ENTRY_COUNT 8
#define ACCOUNT_COUNT 3
#define FEE_RATE 4
#define MIN_BALANCE 5

enum EntryKind { ENTRY_DEBIT = 0, ENTRY_CREDIT = 1, ENTRY_FEE = 2 };

struct Entry {
    int kind;
    int amount;
    int account;
};

const int opening_balance = 20;
static int posted_entries = 0;

static int apply_fee(int amount) {
    int fee = amount / FEE_RATE;
    return fee < 1 ? 1 : fee;
}

static int post_entry(struct Entry *entry, int *balances) {
    int account = entry->account;
    if (account < 0 || account >= ACCOUNT_COUNT) return -1;
    switch (entry->kind) {
        case ENTRY_DEBIT: balances[account] -= entry->amount; break;
        case ENTRY_CREDIT: balances[account] += entry->amount; break;
        case ENTRY_FEE: balances[account] -= apply_fee(entry->amount); break;
        default: return -1;
    }
    posted_entries += 1;
    return account;
}

static int settle(struct Entry *entries, int *balances) {
    int touched = 0;
    for (int i = 0; i < ENTRY_COUNT; ++i)
        if (post_entry(&entries[i], balances) >= 0) touched += 1;
    return touched;
}

int ledger_balance_check(void) {
    struct Entry entries[ENTRY_COUNT] = {
        {ENTRY_CREDIT, 9, 0}, {ENTRY_DEBIT, 4, 1}, {ENTRY_FEE, 12, 2},
        {ENTRY_CREDIT, 6, 1}, {ENTRY_DEBIT, 3, 0}, {ENTRY_FEE, 8, 0},
        {ENTRY_CREDIT, 5, 2}, {ENTRY_DEBIT, 2, 1}
    };
    int balances[ACCOUNT_COUNT] = {opening_balance, opening_balance, opening_balance};
    int touched = settle(entries, balances);
    if (posted_entries != ENTRY_COUNT) return 1;
    if (touched != ENTRY_COUNT) return 2;
    if (balances[0] <= MIN_BALANCE) return 3;
    if (balances[1] <= 0) return 4;
    return 0;
}
