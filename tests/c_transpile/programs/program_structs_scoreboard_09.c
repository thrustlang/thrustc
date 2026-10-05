#define PLAYER_COUNT 5
#define BONUS_CAP 6
#define FLOOR_SCORE 13

struct Player09 {
    int id;
    int score;
    int bonus;
};

const int base_reward_09 = 4;
static int updates_09 = 0;

static int clamp_bonus_09(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_09(struct Player09 *player, int delta) {
    player->bonus = clamp_bonus_09(player->bonus + (delta % 4));
    player->score += delta + player->bonus + base_reward_09;
    updates_09 += 1;
}

static int total_score_09(struct Player09 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player09 players[PLAYER_COUNT] = {
        {1, 10, 2}, {2, 9, 1}, {3, 13, 0}, {4, 8, 3}, {5, 12, 1}
    };
    int deltas[PLAYER_COUNT] = {3, 6, 4, 2, 5};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_09(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_09 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_09(players) <= 77) {
        return 3;
    }
    return 0;
}
