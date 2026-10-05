#define PLAYER_COUNT 5
#define BONUS_CAP 7
#define FLOOR_SCORE 15

struct Player07 {
    int id;
    int score;
    int bonus;
};

const int base_reward_07 = 3;
static int updates_07 = 0;

static int clamp_bonus_07(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_07(struct Player07 *player, int delta) {
    player->bonus = clamp_bonus_07(player->bonus + (delta % 5));
    player->score += delta + player->bonus + base_reward_07;
    updates_07 += 1;
}

static int total_score_07(struct Player07 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player07 players[PLAYER_COUNT] = {
        {1, 14, 0}, {2, 9, 1}, {3, 11, 2}, {4, 12, 1}, {5, 8, 3}
    };
    int deltas[PLAYER_COUNT] = {5, 2, 6, 4, 3};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_07(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_07 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_07(players) <= 76) {
        return 3;
    }
    return 0;
}
