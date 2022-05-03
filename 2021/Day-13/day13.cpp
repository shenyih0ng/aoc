#include <algorithm>
#include <fstream>
#include <iostream>
#include <set>

std::pair<int, int> operator+(const std::pair<int, int> p1,
                              const std::pair<int, int> p2) {
    return std::make_pair<int, int>(p1.first + p2.first, p1.second + p2.second);
}

std::set<std::pair<int, int>> fold(std::set<std::pair<int, int>> dots,
                                   bool fold_x, int mag) {
    std::set<std::pair<int, int>> folded_set;
    std::set<std::pair<int, int>> right_split;

    std::set<std::pair<int, int>>::iterator dotsIt = dots.begin();
    for (; dotsIt != dots.end(); ++dotsIt) {
        if ((fold_x && (*dotsIt).first > mag) ||
            (!fold_x && (*dotsIt).second > mag)) {
            right_split.insert(*dotsIt);
        } else {
            folded_set.insert(*dotsIt);
        }
    }
    std::set<std::pair<int, int>>::iterator rsIt = right_split.begin();
    for (; rsIt != right_split.end(); ++rsIt) {
        if (fold_x) {
            int dot_x = (*rsIt).first;
            folded_set.insert((*rsIt) +
                              std::make_pair(-(2 * (dot_x - mag)), 0));
        } else {
            int dot_y = (*rsIt).second;
            folded_set.insert((*rsIt) +
                              std::make_pair(0, -(2 * (dot_y - mag))));
        }
    }

    return folded_set;
}

void display_dots(std::set<std::pair<int, int>> &dots) {
    std::set<std::pair<int, int>>::iterator dotsIt = dots.begin();
    std::set<std::pair<int, int>>::iterator dotsItend = dots.end();
    --dotsItend; // get the last
    int min_x = (*dotsIt).first;
    int max_x = (*dotsItend).first;
    int min_y = (*dotsIt).second; // init
    int max_y = (*dotsIt).second; // init

    for (; dotsIt != dots.end(); ++dotsIt) {
        min_y = std::min(min_y, (*dotsIt).second);
        max_y = std::max(max_y, (*dotsIt).second);
    }

    for (int c = min_y; c <= max_y; ++c) {
        for (int r = min_x; r <= max_x; ++r) {
            if (dots.find(std::make_pair(r, c)) != dots.end()) {
                std::cout << "*";
            } else {
                std::cout << ".";
            }
        }
        std::cout << std::endl;
    }
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    std::set<std::pair<int, int>> dots;
    while (getline(input_stream, line) && line != "") {
        int row = std::stoi(line.substr(0, line.find(",")));
        int col = std::stoi(line.substr(line.find(",") + 1));
        dots.insert(std::make_pair(row, col));
    }

    // instructions
    bool part1_done = false;
    while (getline(input_stream, line)) {
        int delim_pos = line.find("=");
        bool fold_x = line[delim_pos - 1] == 'x';
        int mag = std::stoi(line.substr(delim_pos + 1));
        dots = fold(dots, fold_x, mag);
        if (!part1_done) {
            std::cout << "Part 1: " << dots.size() << std::endl;
            part1_done = true;
        }
    }

    std::cout << "Part 2: " << std::endl;
    display_dots(dots);

    return EXIT_SUCCESS;
}