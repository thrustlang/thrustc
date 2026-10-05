#define PLAYER_COUNT 6
#define BONUS_CAP 5
#define FLOOR_SCORE 12

struct Player06 {
    int id;
    int score;
    int bonus;
};

const int base_reward_06 = 4;
static int updates_06 = 0;

static int clamp_bonus_06(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_06(struct Player06 *player, int delta) {
    player->bonus = clamp_bonus_06(player->bonus + (delta % 4));
    player->score += delta + player->bonus + base_reward_06;
    updates_06 += 1;
}

static int total_score_06(struct Player06 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player06 players[PLAYER_COUNT] = {
        {1, 10, 1}, {2, 7, 2}, {3, 13, 1}, {4, 9, 0}, {5, 11, 3}, {6, 8, 2}
    };
    int deltas[PLAYER_COUNT] = {4, 5, 3, 2, 6, 1};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_06(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_06 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_06(players) <= 79) {
        return 3;
    }
    return 0;
}
