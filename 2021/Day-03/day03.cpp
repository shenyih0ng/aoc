#include <fstream>
#include <iostream>
#include <vector>

int binary_to_int(const std::string binary_str) {
    return std::stoi(binary_str, 0, 2);
}

std::vector<int> get_ones_freq(std::vector<std::string> &vals) {
    std::vector<int> freq(vals[0].size(), 0);
    std::vector<std::string>::iterator vIt = vals.begin();
    for (; vIt != vals.end(); ++vIt) {
        std::string val = *vIt;
        for (int idx = 0; idx < val.size(); ++idx) {
            freq[idx] += val[idx] == '1';
        }
    }

    return freq;
}

char get_most_common_bit(std::vector<int> &ones_freq, int total, int pos) {
    int num_ones = ones_freq[pos];
    int num_zeros = total - num_ones;
    return num_ones == num_zeros ? '1' : num_ones > num_zeros ? '1' : '0';
}

char get_least_common_bit(std::vector<int> &ones_freq, int total, int pos) {
    char most_common_bit = get_most_common_bit(ones_freq, total, pos);
    return most_common_bit == '1' ? '0' : '1';
}

std::string get_epsilon_rate(std::vector<int> &ones_freq, int total) {
    std::string binary_str;
    for (int idx = 0; idx < ones_freq.size(); ++idx) {
        binary_str.push_back(get_least_common_bit(ones_freq, total, idx));
    }

    return binary_str;
}

std::string get_gamma_rate(std::vector<int> &ones_freq, int total) {
    std::string binary_str;
    for (int idx = 0; idx < ones_freq.size(); ++idx) {
        binary_str.push_back(get_most_common_bit(ones_freq, total, idx));
    }

    return binary_str;
}

std::string get_oxygen_rating(std::vector<std::string> &vals, int pos = 0) {
    if (vals.size() == 1) {
        return vals[0];
    } else {
        std::vector<std::string> filtered;
        std::vector<int> ones_freq = get_ones_freq(vals);
        char most_common = get_most_common_bit(ones_freq, vals.size(), pos);
        std::copy_if(vals.begin(), vals.end(), std::back_inserter(filtered),
                     [pos, most_common](std::string x) {
                         return x[pos] == most_common;
                     });

        return get_oxygen_rating(filtered, pos + 1);
    }
}

std::string get_co2_rating(std::vector<std::string> &vals, int pos = 0) {
    if (vals.size() == 1) {
        return vals[0];
    } else {
        std::vector<std::string> filtered;
        std::vector<int> ones_freq = get_ones_freq(vals);
        char least_common = get_least_common_bit(ones_freq, vals.size(), pos);
        std::copy_if(vals.begin(), vals.end(), std::back_inserter(filtered),
                     [pos, least_common](std::string x) {
                         return x[pos] == least_common;
                     });

        return get_co2_rating(filtered, pos + 1);
    }
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    std::vector<std::string> vals;
    while (getline(input_stream, line)) {
        vals.push_back(line);
    }

    int total = vals.size();
    std::vector<int> num_ones = get_ones_freq(vals);
    std::cout << "Part 1: "
              << binary_to_int(get_epsilon_rate(num_ones, total)) *
                     binary_to_int(get_gamma_rate(num_ones, total))
              << std::endl;

    std::cout << "Part 2: "
              << binary_to_int(get_oxygen_rating(vals)) *
                     binary_to_int(get_co2_rating(vals))
              << std::endl;

    return EXIT_SUCCESS;
}