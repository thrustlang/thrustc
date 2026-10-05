#define PLAYER_COUNT 5
#define BONUS_CAP 6
#define FLOOR_SCORE 10

struct Player01 {
    int id;
    int score;
    int bonus;
};

const int base_reward_01 = 3;
static int updates_01 = 0;

static int clamp_bonus_01(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_01(struct Player01 *player, int delta) {
    player->bonus = clamp_bonus_01(player->bonus + (delta % 4));
    player->score += delta + player->bonus + base_reward_01;
    updates_01 += 1;
}

static int total_score_01(struct Player01 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player01 players[PLAYER_COUNT] = {
        {1, 10, 1}, {2, 8, 2}, {3, 12, 0}, {4, 7, 3}, {5, 11, 1}
    };
    int deltas[PLAYER_COUNT] = {4, 3, 5, 2, 6};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_01(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_01 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_01(players) <= 70) {
        return 3;
    }
    return 0;
}
