#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <vector>

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");

    std::vector<int> positions;
    std::string line;
    while (getline(input_stream, line, ',')) {
        positions.push_back(std::stoi(line));
    }

    // sort positions
    std::sort(positions.begin(), positions.end(),
              [](int i, int j) { return i < j; });
    // get median
    int median = positions[positions.size() / 2];
    // get positions - median
    int fuel =
        std::accumulate(positions.begin(), positions.end(), 0,
                        [median](int x, int y) { return x + abs(y - median); });

    double sum = std::accumulate(positions.begin(), positions.end(), 0);
    int upper_mean = ceil(sum / positions.size());
    int lower_mean = floor(sum / positions.size());
    std::function<std::function<int(int, int)>(int)> calc_fuel =
        [](int final_pos) {
            return [final_pos](int x, int y) {
                return x + (abs(y - final_pos) * (abs(y - final_pos) + 1)) / 2;
            };
        };
    // the computation is fast enough that there is no need for extra heuristics
    int modified_fuel_upper = std::accumulate(
        positions.begin(), positions.end(), 0, calc_fuel(upper_mean));
    int modified_fuel_lower = std::accumulate(
        positions.begin(), positions.end(), 0, calc_fuel(lower_mean));
    int modified_fuel = std::min(modified_fuel_lower, modified_fuel_upper);

    std::cout << "Part 1: " << fuel << std::endl;
    std::cout << "Part 2: " << modified_fuel << std::endl;

    return EXIT_SUCCESS;
}