#define PLAYER_COUNT 5
#define BONUS_CAP 6
#define FLOOR_SCORE 13

struct Player05 {
    int id;
    int score;
    int bonus;
};

const int base_reward_05 = 5;
static int updates_05 = 0;

static int clamp_bonus_05(int value) {
    if (value < 0) {
        return 0;
    }
    if (value > BONUS_CAP) {
        return BONUS_CAP;
    }
    return value;
}

static void apply_round_05(struct Player05 *player, int delta) {
    player->bonus = clamp_bonus_05(player->bonus + (delta % 3));
    player->score += delta + player->bonus + base_reward_05;
    updates_05 += 1;
}

static int total_score_05(struct Player05 *players) {
    int total = 0;
    for (int i = 0; i < PLAYER_COUNT; ++i) {
        total += players[i].score;
    }
    return total;
}

int main(void) {
    struct Player05 players[PLAYER_COUNT] = {
        {1, 9, 2}, {2, 11, 1}, {3, 10, 0}, {4, 8, 3}, {5, 12, 2}
    };
    int deltas[PLAYER_COUNT] = {6, 2, 5, 4, 3};
    int best = 0;

    for (int i = 0; i < PLAYER_COUNT; ++i) {
        apply_round_05(&players[i], deltas[i]);
        if (players[i].score > best) {
            best = players[i].score;
        }
    }

    if (updates_05 != PLAYER_COUNT) {
        return 1;
    }
    if (best <= FLOOR_SCORE) {
        return 2;
    }
    if (total_score_05(players) <= 78) {
        return 3;
    }
    return 0;
}
