#include <fstream>
#include <iostream>
#include <regex>
#include <vector>

struct Cuboid {
    bool valid;
    bool to_add;
    int64_t min_x, max_x, min_y, max_y, min_z, max_z;
};

Cuboid intersect(Cuboid c1, Cuboid c2) {
    Cuboid intersect_cuboid = {true,
                               !(c1.to_add),
                               std::max(c1.min_x, c2.min_x),
                               std::min(c1.max_x, c2.max_x),
                               std::max(c1.min_y, c2.min_y),
                               std::min(c1.max_y, c2.max_y),
                               std::max(c1.min_z, c2.min_z),
                               std::min(c1.max_z, c2.max_z)};

    intersect_cuboid.valid =
        !(intersect_cuboid.min_x > intersect_cuboid.max_x ||
          intersect_cuboid.min_y > intersect_cuboid.max_y ||
          intersect_cuboid.min_z > intersect_cuboid.max_z);

    return intersect_cuboid;
}

int64_t get_volume(Cuboid c) {
    if (!(c.valid)) {
        return 0;
    }

    int64_t volume = (c.max_x - c.min_x + 1) * (c.max_y - c.min_y + 1) *
                     (c.max_z - c.min_z + 1);

    return c.to_add ? volume : -1 * volume;
}

bool within_bound(Cuboid c, Cuboid bound) {
    if (!bound.valid) {
        return true;
    }

    return intersect(c, bound).valid;
}

int64_t get_num_voxel_on(std::vector<Cuboid> &instructions, Cuboid bound) {
    std::vector<Cuboid> store;

    for (int idx = 0; idx < instructions.size(); idx++) {
        std::vector<Cuboid> temp_store;
        Cuboid instruct_cuboid = instructions[idx];

        if (instruct_cuboid.to_add && within_bound(instruct_cuboid, bound)) {
            temp_store.push_back(instruct_cuboid);
        }

        for (int sIdx = 0; sIdx < store.size(); sIdx++) {
            Cuboid store_cuboid = store[sIdx];
            Cuboid intersect_cuboid = intersect(store_cuboid, instruct_cuboid);
            if (intersect_cuboid.valid) {
                temp_store.push_back(intersect_cuboid);
            }
        }

        store.insert(store.begin(), temp_store.begin(), temp_store.end());
    }

    int64_t num_voxel_on = 0;
    for (int idx = 0; idx < store.size(); idx++) {
        Cuboid cuboid = store[idx];
        num_voxel_on += get_volume(cuboid);
    }

    return num_voxel_on;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string cuboid_str;

    std::vector<Cuboid> cuboids;
    while (getline(input_stream, cuboid_str)) {
        std::smatch cuboid_regex_match;
        std::regex_search(
            cuboid_str, cuboid_regex_match,
            std::regex(
                "^(.+) x=(.+)\\.\\.(.+),y=(.+)\\.\\.(.+),z=(.+)\\.\\.(.+)$"));

        Cuboid cuboid = {true,
                         cuboid_regex_match[1] == "on",
                         std::stoi(cuboid_regex_match[2]),
                         std::stoi(cuboid_regex_match[3]),
                         std::stoi(cuboid_regex_match[4]),
                         std::stoi(cuboid_regex_match[5]),
                         std::stoi(cuboid_regex_match[6]),
                         std::stoi(cuboid_regex_match[7])};

        cuboids.push_back(cuboid);
    }

    // Bound of -50 to 50 in all axis
    Cuboid bound = {true, false, -50, 50, -50, 50, -50, 50};
    std::cout << "Part 1: " << get_num_voxel_on(cuboids, bound) << std::endl;

    bound.valid = false; // disable bound
    std::cout << "Part 2: " << get_num_voxel_on(cuboids, bound) << std::endl;

    return EXIT_SUCCESS;
}
