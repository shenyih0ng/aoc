#include <assert.h>

#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <vector>

// 3D transforms
int xform_reflect[8][3] = {
    {1, 1, 1},  {1, 1, -1},  {1, -1, 1},  {1, -1, -1},
    {-1, 1, 1}, {-1, 1, -1}, {-1, -1, 1}, {-1, -1, -1},
};

int xform_rotate[6][3] = {{0, 1, 2}, {0, 2, 1}, {1, 0, 2},
                          {1, 2, 0}, {2, 1, 0}, {2, 0, 1}};

struct Point {
    int x = 0;
    int y = 0;
    int z = 0;
} origin;

typedef std::tuple<int, int, int> point_tpl; // tuple for ordering
typedef std::pair<int, int> transform_t;
typedef Point offset_t;

struct Scanner {
    std::vector<Point> points;
    transform_t transforms;
    offset_t offset;
};

Point transform(Point &p, transform_t transform, Point &offset) {
    const int vals[3] = {p.x, p.y, p.z};
    int *reflect = xform_reflect[transform.first],
        *rotate = xform_rotate[transform.second];

    return {vals[rotate[0]] * reflect[0] + offset.x,
            vals[rotate[1]] * reflect[1] + offset.y,
            vals[rotate[2]] * reflect[2] + offset.z};
}

std::pair<transform_t, offset_t> find_overlap_config(Scanner *s1, Scanner *s2,
                                                     int threshold) {
    for (int reflect_idx = 0; reflect_idx < 8; reflect_idx++) {
        for (int rotate_idx = 0; rotate_idx < 6; rotate_idx++) {
            std::map<point_tpl, int> diff_map;
            for (Point p1 : s1->points) {
                Point norm_p1 = transform(p1, s1->transforms, s1->offset);
                for (Point p2 : s2->points) {
                    Point norm_p2 = transform(
                        p2, std::make_pair(reflect_idx, rotate_idx), origin);
                    diff_map[std::make_tuple(norm_p1.x - norm_p2.x,
                                             norm_p1.y - norm_p2.y,
                                             norm_p1.z - norm_p2.z)]++;
                }
            }
            for (std::map<point_tpl, int>::iterator it = diff_map.begin();
                 it != diff_map.end(); it++) {
                if (it->second >= threshold) {
                    point_tpl p_t = it->first;
                    offset_t o = {std::get<0>(p_t), std::get<1>(p_t),
                                  std::get<2>(p_t)};
                    return std::make_pair(
                        std::make_pair(reflect_idx, rotate_idx), o);
                }
            }
        }
    }

    return std::make_pair(std::make_pair(-1, -1), origin);
}

void resolve_scanners(std::vector<Scanner *> &scanners, int threshold) {
    std::set<int> verified_scanners = {0};
    while (verified_scanners.size() < scanners.size()) {
        for (int s_idx = 0; s_idx < scanners.size(); s_idx++) {
            if (verified_scanners.find(s_idx) != verified_scanners.end()) {
                continue;
            }

            for (int v_idx : verified_scanners) {
                std::pair<transform_t, offset_t> overlap_config =
                    find_overlap_config(scanners[v_idx], scanners[s_idx],
                                        threshold);

                if (overlap_config.first.first == -1) {
                    continue;
                }

                scanners[s_idx]->transforms = overlap_config.first;
                scanners[s_idx]->offset = overlap_config.second;
                verified_scanners.insert(s_idx);
                break;
            }
        }
    }
}

int find_num_beacons(std::vector<Scanner *> &scanners) {
    std::set<point_tpl> beacon_coords;

    for (Scanner *s : scanners) {
        for (Point p : s->points) {
            Point n_p = transform(p, s->transforms, s->offset);
            beacon_coords.insert(std::make_tuple(n_p.x, n_p.y, n_p.z));
        }
    }

    return beacon_coords.size();
}

int find_largest_dist(std::vector<Scanner *> &scanners) {
    int max_dist = -1;
    for (int i = 0; i < scanners.size(); i++) {
        offset_t s1_o = scanners[i]->offset;
        for (int j = i + 1; j < scanners.size(); j++) {
            offset_t s2_o = scanners[j]->offset;
            max_dist = std::max(max_dist, std::abs(s1_o.x - s2_o.x) +
                                              std::abs(s1_o.y - s2_o.y) +
                                              std::abs(s1_o.z - s2_o.z));
        }
    }

    return max_dist;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    int scan_idx = -1;
    std::vector<Scanner *> scanners;

    std::regex coord_regex("(-?\\d+),(-?\\d+),(-?\\d+)");
    while (getline(input_stream, line)) {
        if (line == "")
            continue;
        if (line.substr(0, 3) == "---") {
            scanners.push_back(new Scanner());
            scan_idx++;
            continue;
        }

        std::smatch matches;
        assert(std::regex_search(line, matches, coord_regex));
        assert(matches.size() == 4);

        scanners[scan_idx]->points.push_back({std::stoi(matches[1]),
                                              std::stoi(matches[2]),
                                              std::stoi(matches[3])});
    }

    // scanner 0 as base ref
    scanners[0]->transforms = std::make_pair(0, 0);
    scanners[0]->offset = origin;
    resolve_scanners(scanners, 12);

    std::cout << "Part 1: " << find_num_beacons(scanners) << std::endl;
    std::cout << "Part 2: " << find_largest_dist(scanners) << std::endl;

    return EXIT_SUCCESS;
}

