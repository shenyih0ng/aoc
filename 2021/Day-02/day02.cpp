#include <fstream>
#include <iostream>
#include <sstream>

std::pair<int, int> operator+(const std::pair<int, int> p1,
                              const std::pair<int, int> p2) {
    return std::make_pair<int, int>(p1.first + p2.first, p1.second + p2.second);
}

std::pair<int, int> cmd_to_vect(std::string cmd) {
    std::istringstream ss(cmd);
    std::string cmd_str, cmd_val;
    std::getline(ss, cmd_str, ' ');
    std::getline(ss, cmd_val, ' ');

    if (cmd_str == "forward") {
        return std::make_pair<int, int>(std::stoi(cmd_val), 0);
    } else if (cmd_str == "down") {
        return std::make_pair<int, int>(0, std::stoi(cmd_val));
    } else {
        return std::make_pair<int, int>(0, -1 * std::stoi(cmd_val));
    }
}

bool is_forward_vect(const std::pair<int, int> vect) { return vect.first != 0; }

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    std::pair<int, int> dive_result = std::make_pair<int, int>(0, 0);
    std::pair<int, int> dive_aim_result = std::make_pair<int, int>(0, 0);
    int aim = 0;
    while (getline(input_stream, line)) {
        std::pair<int, int> cmd_vect = cmd_to_vect(line);
        if (!is_forward_vect(cmd_vect)) {
            aim += cmd_vect.second;
        } else {
            dive_aim_result = cmd_vect + dive_aim_result;
            dive_aim_result = dive_aim_result +
                              std::make_pair<int, int>(0, aim * cmd_vect.first);
        }

        dive_result = dive_result + cmd_vect;
    }

    std::cout << "Part 1: " << dive_result.first * dive_result.second
              << std::endl;
    std::cout << "Part 2: " << dive_aim_result.first * dive_aim_result.second
              << std::endl;

    return EXIT_SUCCESS;
}
