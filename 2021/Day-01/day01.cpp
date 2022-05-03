#include <fstream>
#include <iostream>
#include <vector>

int find_num_increase(std::vector<int> vals) {
    std::vector<int>::iterator vIt = vals.begin();
    int count = 0;
    int prev = *vIt;
    for (; vIt != vals.end(); ++vIt) {
        count += *vIt > prev;
        prev = *vIt;
    }

    return count;
}

std::vector<int> to_sliding_window(int win_size, std::vector<int> vals) {
    std::vector<int> result;
    for (int idx = 0; idx <= vals.size() - win_size; ++idx) {
        int sum = 0;
        for (int inc = 0; inc < win_size; ++inc) {
            sum += vals[idx + inc];
        }
        result.push_back(sum);
    }

    return result;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");

    std::vector<int> vals;
    std::string line;
    while (getline(input_stream, line)) {
        int curr = stoi(line);
        vals.push_back(curr);
    }

    std::cout << "Part 1: " << find_num_increase(vals) << std::endl;
    std::cout << "Part 2: " << find_num_increase(to_sliding_window(3, vals))
              << std::endl;

    return EXIT_SUCCESS;
}