#include <fstream>
#include <iostream>
#include <map>
#include <tuple>

typedef std::function<int()> dice;
typedef std::map<std::tuple<int, int, int, int, bool>,
                 std::pair<uint64_t, uint64_t>>
    dirac_cache;

const int DIRAC_FREQ[10] = {0, 0, 0, 1, 3, 6, 7, 6, 3, 1};

dice create_det_dice() {
    int dice_start = 1;
    int dice_max = 100;

    return [dice_start, dice_max]() mutable {
        int result = dice_start % dice_max;
        dice_start++;

        return result;
    };
}

int get_roll(dice &d, int num_rolls) {
    int roll_value = 0;
    for (int count = 0; count < num_rolls; count++) {
        roll_value += d();
    }

    return roll_value;
}

// Part 1
// Returns (losing score * num dice rolls)
int play_game(int p1_pos, int p2_pos, int num_rolls, int track_len) {
    int p1_score = 0;
    int p1_curr_pos = (p1_pos - 1) % 10;
    int p2_score = 0;
    int p2_curr_pos = (p2_pos - 1) % 10;

    int total_rolls = 0;
    bool p1_turn = true;
    dice det_dice = create_det_dice();
    while (p1_score < 1000 && p2_score < 1000) {
        if (p1_turn) {
            p1_curr_pos =
                (get_roll(det_dice, num_rolls) + p1_curr_pos) % track_len;
            p1_score += (p1_curr_pos + 1);
        } else {
            p2_curr_pos =
                (get_roll(det_dice, num_rolls) + p2_curr_pos) % track_len;
            p2_score += (p2_curr_pos + 1);
        }

        p1_turn = !p1_turn;
        total_rolls += num_rolls;
    }

    if (p1_score >= 1000) {
        return total_rolls * p2_score;
    } else {
        return total_rolls * p1_score;
    }
}

// Part 2
// Returns (p1_universe_wins, p2_universe_wins)
std::pair<uint64_t, uint64_t> play_dirac(int p1_pos, int p2_pos, int p1_score,
                                         int p2_score, bool p1_turn,
                                         dirac_cache &cache) {
    dirac_cache::iterator cIt = cache.find(
        std::make_tuple(p1_pos, p2_pos, p1_score, p2_score, p1_turn));
    if (cIt != cache.end()) {
        return cIt->second;
    } else if (p1_score >= 21) {
        return std::make_pair(1, 0);
    } else if (p2_score >= 21) {
        return std::make_pair(0, 1);
    } else {
        uint64_t p1_universe_wins = 0;
        uint64_t p2_universe_wins = 0;

        for (int roll_value = 3; roll_value <= 9; roll_value++) {
            std::pair<uint64_t, uint64_t> result;
            if (p1_turn) {
                int p1_new_pos = (p1_pos + roll_value) % 10;
                int p1_new_score = p1_score + p1_new_pos + 1;
                result = play_dirac(p1_new_pos, p2_pos, p1_new_score, p2_score,
                                    !p1_turn, cache);
            } else {
                int p2_new_pos = (p2_pos + roll_value) % 10;
                int p2_new_score = p2_score + p2_new_pos + 1;
                result = play_dirac(p1_pos, p2_new_pos, p1_score, p2_new_score,
                                    !p1_turn, cache);
            }

            p1_universe_wins += (result.first * DIRAC_FREQ[roll_value]);
            p2_universe_wins += (result.second * DIRAC_FREQ[roll_value]);
        }

        cache.insert(std::make_pair(
            std::make_tuple(p1_pos, p2_pos, p1_score, p2_score, p1_turn),
            std::make_pair(p1_universe_wins, p2_universe_wins)));

        return std::make_pair(p1_universe_wins, p2_universe_wins);
    }
}

uint64_t play_dirac(int p1_pos, int p2_pos) {
    dirac_cache cache;
    std::pair<uint64_t, uint64_t> result =
        play_dirac((p1_pos - 1) % 10, (p2_pos - 1) % 10, 0, 0, true, cache);

    return std::max(result.first, result.second);
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");

    std::string p1_str, p2_str;
    int p1_pos, p2_pos;

    getline(input_stream, p1_str);
    getline(input_stream, p2_str);

    p1_pos = std::stoi(p1_str.substr(p1_str.find(":") + 2));
    p2_pos = std::stoi(p2_str.substr(p2_str.find(":") + 2));

    dirac_cache cache;
    std::cout << "Part 1: " << play_game(p1_pos, p2_pos, 3, 10) << std::endl;
    std::cout << "Part 2: " << play_dirac(p1_pos, p2_pos) << std::endl;

    return EXIT_SUCCESS;
}
