#include <fstream>
#include <iostream>

// [min_x, max_x, min_y, max_y]
typedef int target_area_t[4];

void parse_target_area_str(std::string target_area_str,
                           target_area_t target_area) {
    std::string x_range = target_area_str.substr(
        target_area_str.find('x') + 2,
        target_area_str.find(',') - target_area_str.find('x') - 2);
    std::string y_range = target_area_str.substr(target_area_str.find('y') + 2);

    target_area[0] = std::stoi(x_range.substr(0, x_range.find("..")));
    target_area[1] = std::stoi(x_range.substr(x_range.find("..") + 2));
    target_area[2] = std::stoi(y_range.substr(0, y_range.find("..")));
    target_area[3] = std::stoi(y_range.substr(y_range.find("..") + 2));
}

int find_max_vert_height(target_area_t target_area) {
    /*
    Since we are starting at (0,0), we can sure that when it reaches (0,0)
    again, vy = -(initial vy) Therefore we just need to start from ((initial vy)
    + 1) + (initial vy + 2) ... within y_range
    */

    return (abs(target_area[2]) * (abs(target_area[2]) + 1)) / 2 +
           target_area[2];
}

int find_num_possible(target_area_t target_area) {
    int num_possible = 0;
    for (int vx = 1; vx <= target_area[1]; ++vx) {
        for (int vy = target_area[2]; vy < abs(target_area[2]); ++vy) {
            bool reached = false;
            bool exceeded = false;
            int tick = 0;
            int x = 0;
            int y = 0;
            while (!reached && !exceeded) {
                x += std::max(vx - tick, 0);
                y += vy - tick;
                reached = (x <= target_area[1] && x >= target_area[0]) &&
                          (y <= target_area[3] && y >= target_area[2]);
                exceeded = x > target_area[1] || y < target_area[2];
                ++tick;
            }

            num_possible += reached;
        }
    }

    return num_possible;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string target_area_str;

    getline(input_stream, target_area_str);
    target_area_t target_area;
    parse_target_area_str(target_area_str, target_area);

    std::cout << "Part 1: " << find_max_vert_height(target_area) << std::endl;
    std::cout << "Part 2: " << find_num_possible(target_area) << std::endl;

    return EXIT_SUCCESS;
}