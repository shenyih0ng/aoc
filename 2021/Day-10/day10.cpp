#include <algorithm>
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <vector>

const std::unordered_map<char, int> ERROR_SCORES = {
    {')', 3}, {']', 57}, {'}', 1197}, {'>', 25137}};

const std::unordered_map<char, int> COMPLETION_SCORES = {
    {')', 1}, {']', 2}, {'}', 3}, {'>', 4}};

bool is_open(char c) { return c == '(' || c == '[' || c == '{' || c == '<'; }
bool is_close(char c) { return !is_open(c); }

bool is_legal_close(char open, char close) {
    return (open == '(' && close == ')') || abs(open - close) == 2;
}

char open_to_close(char open) { return open == '(' ? ')' : open + 2; }

char find_first_illegal_char(std::string nav_str) {
    std::vector<char> store;
    for (int idx = 0; idx < nav_str.size(); ++idx) {
        if (is_close(nav_str[idx])) {
            if (!is_legal_close(store.back(), nav_str[idx])) {
                return nav_str[idx];
            }
            store.pop_back();
        } else {
            store.push_back(nav_str[idx]);
        }
    }

    return '\0';
}

std::vector<char> get_completion(std::string incomplete_nav_str) {
    std::vector<char> remaining_open;
    for (int idx = 0; idx < incomplete_nav_str.size(); ++idx) {
        if (is_close(incomplete_nav_str[idx])) {
            remaining_open.pop_back();
        } else {
            remaining_open.push_back(incomplete_nav_str[idx]);
        }
    }

    std::reverse(remaining_open.begin(), remaining_open.end());
    std::vector<char> completion;
    std::transform(remaining_open.begin(), remaining_open.end(),
                   std::back_inserter(completion),
                   [](char c) { return open_to_close(c); });

    return completion;
}

long int get_completion_score(std::vector<char> completion_str) {
    long int score = 0;
    std::vector<char>::iterator it = completion_str.begin();
    for (; it < completion_str.end(); ++it) {
        score = (score * 5) + COMPLETION_SCORES.at(*it);
    }

    return score;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    int error_score = 0;
    std::vector<long int> completion_scores;
    while (getline(input_stream, line)) {
        char illegal_char = find_first_illegal_char(line);
        if (illegal_char != '\0') {
            // note that since ERROR_SCORE is a const [] is not supported
            error_score += ERROR_SCORES.at(illegal_char);
        } else {
            completion_scores.push_back(
                get_completion_score(get_completion(line)));
        }
    }

    std::cout << "Part 1: " << error_score << std::endl;
    std::sort(completion_scores.begin(), completion_scores.end());
    std::cout << "Part 2: "
              << completion_scores[(completion_scores.size() - 1) / 2]
              << std::endl;

    return EXIT_SUCCESS;
}