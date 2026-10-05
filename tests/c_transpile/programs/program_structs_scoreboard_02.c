#define PLAYER_COUNT 6
#define BONUS_CAP 5
#define FLOOR_SCORE 12

struct Player02 {
    int id;
    int score;
    int bonus;
};

const int base_reward_02 = 2;
static int updates_02 = 0;

static int clamp_bonus_02(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_02(struct Player02 *player, int delta) {
    player->bonus = clamp_bonus_02(player->bonus + (delta % 3));
    player->score += delta + player->bonus + base_reward_02;
    updates_02 += 1;
}

static int total_score_02(struct Player02 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player02 players[PLAYER_COUNT] = {
        {1, 9, 0}, {2, 13, 1}, {3, 7, 2}, {4, 11, 1}, {5, 10, 3}, {6, 8, 0}
    };
    int deltas[PLAYER_COUNT] = {3, 5, 2, 4, 6, 1};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_02(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_02 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_02(players) <= 75) {
        return 3;
    }
    return 0;
}
