#include <fstream>
#include <iostream>
#include <map>
#include <vector>

typedef std::map<std::string, std::string> instruct_t;
typedef std::map<char, int64_t> polymer_freq_t;
typedef std::map<std::string, int64_t> polymer_pair_freq_t;

void generate_polymer(polymer_freq_t &p_freq, polymer_pair_freq_t p_pair_freq,
                      instruct_t &instructs, int end, int count = 0) {
    if (count >= end) {
        return;
    } else {
        polymer_pair_freq_t next_pair_freq;
        polymer_pair_freq_t::iterator pairIt = p_pair_freq.begin();
        for (; pairIt != p_pair_freq.end(); ++pairIt) {
            std::string pair = (*pairIt).first;
            int64_t pair_count = (*pairIt).second;
            instruct_t::iterator add = instructs.find(pair);
            if (add != instructs.end()) {
                std::string add_char = (*add).second;
                p_freq[add_char[0]] += pair_count;
                next_pair_freq[pair[0] + add_char] += pair_count;
                next_pair_freq[add_char + pair[1]] += pair_count;
                next_pair_freq[pair] -= pair_count;
            }
            next_pair_freq[pair] += pair_count;
        }

        generate_polymer(p_freq, next_pair_freq, instructs, end, count + 1);
    }
}

std::vector<int64_t> get_sorted_polymer_freq(polymer_freq_t polymer_freq) {
    std::vector<int64_t> polymer_freq_vect;
    polymer_freq_t::iterator it = polymer_freq.begin();
    for (; it != polymer_freq.end(); ++it) {
        polymer_freq_vect.push_back((*it).second);
    }

    std::sort(polymer_freq_vect.begin(), polymer_freq_vect.end());
    return polymer_freq_vect;
}

// find diff between most common and least common
int64_t get_diff(polymer_freq_t polymer_freq) {
    std::vector<int64_t> sorted_polymer_freq =
        get_sorted_polymer_freq(polymer_freq);
    return sorted_polymer_freq[sorted_polymer_freq.size() - 1] -
           sorted_polymer_freq[0];
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");

    std::string polymer;
    getline(input_stream, polymer);

    instruct_t instructions;
    std::string line;
    getline(input_stream, line); // white space
    while (getline(input_stream, line)) {
        std::string pair = line.substr(0, line.find(" -> "));
        std::string add = line.substr(line.find(" -> ") + 4);
        instructions.insert(std::make_pair(pair, add));
    }

    polymer_freq_t p_freqs;
    polymer_pair_freq_t p_pair_freqs;
    for (int idx = 0; idx < polymer.size() - 1; ++idx) {
        std::string p = polymer.substr(idx, 2);
        p_freqs[p[0]] += 1;
        p_pair_freqs[p] += 1;
    }
    p_freqs[polymer[polymer.size() - 1]] += 1;

    polymer_freq_t p1_p_freqs = p_freqs;
    polymer_pair_freq_t p1_p_pair_freqs = p_pair_freqs;
    generate_polymer(p1_p_freqs, p1_p_pair_freqs, instructions, 10);
    std::cout << "Part 1: " << get_diff(p1_p_freqs) << std::endl;

    polymer_freq_t p2_p_freqs = p_freqs;
    polymer_pair_freq_t p2_p_pair_freqs = p_pair_freqs;
    generate_polymer(p2_p_freqs, p2_p_pair_freqs, instructions, 40);
    std::cout << "Part 2: " << get_diff(p2_p_freqs) << std::endl;

    return EXIT_SUCCESS;
}