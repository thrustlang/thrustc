#define PLAYER_COUNT 5
#define BONUS_CAP 7
#define FLOOR_SCORE 14

struct Player03 {
    int id;
    int score;
    int bonus;
};

const int base_reward_03 = 4;
static int updates_03 = 0;

static int clamp_bonus_03(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_03(struct Player03 *player, int delta) {
    player->bonus = clamp_bonus_03(player->bonus + (delta % 5));
    player->score += delta + player->bonus + base_reward_03;
    updates_03 += 1;
}

static int total_score_03(struct Player03 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player03 players[PLAYER_COUNT] = {
        {1, 12, 1}, {2, 10, 0}, {3, 8, 2}, {4, 14, 3}, {5, 9, 1}
    };
    int deltas[PLAYER_COUNT] = {5, 4, 3, 6, 2};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_03(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_03 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_03(players) <= 80) {
        return 3;
    }
    return 0;
}
