#include <fstream>
#include <iostream>
#include <map>
#include <sstream>

uint64_t get_num_lf(int lf, int day, bool new_lf, int end) {
    if (day > end) {
        return 0;
    } else {
        return new_lf + get_num_lf(8, day + lf + 1, true, end) +
               get_num_lf(6, day + lf + 1, false, end);
    }
}

/*
Because of the way the function is implemented, memoization will not improve
runtime that much.

This is because one of the variables that is part of the "function state" is
`day` Thus, we will not see repeated evaluations since `day` is strictly
increasing. This means that as long as the initial state differs, there will be
no repeated evaluations based on how our function arguments is defined.

Therefore, we can only memoize the results of the initial state, and reuse the
results when we have lantern fishes that have the same initial state.
*/
uint64_t get_total_num_lf(std::string lf_initial_state, int end) {
    uint64_t total = 0;
    std::map<int, uint64_t> cache;

    std::string lf_str;
    std::stringstream lf_state_ss(lf_initial_state);
    while (getline(lf_state_ss, lf_str, ',')) {
        int lf = std::stoi(lf_str);
        if (cache.find(lf) != cache.end()) {
            total += cache[lf];
        } else {
            uint64_t num_lf = get_num_lf(lf, 0, true, end);
            cache[lf] = num_lf;
            total += num_lf;
        }
    }

    return total;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string lf_str;
    getline(input_stream, lf_str);

    // runtime: ~1.5 min
    std::cout << "Part 1: " << get_total_num_lf(lf_str, 80) << std::endl;
    std::cout << "Part 2: " << get_total_num_lf(lf_str, 256) << std::endl;

    return -1;
}