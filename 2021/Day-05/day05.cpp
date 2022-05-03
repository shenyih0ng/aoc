#include <fstream>
#include <iostream>
#include <set>
#include <vector>

const std::string LINE_DELIM = " -> ";

typedef std::pair<int, int> point_t;
typedef std::pair<point_t, point_t> line_t;

point_t get_start(std::string line_str) {
    std::string start_str = line_str.substr(0, line_str.find(LINE_DELIM));
    size_t delim_pos = start_str.find(',');
    return std::make_pair(std::stoi(start_str.substr(0, delim_pos)),
                          std::stoi(start_str.substr(delim_pos + 1)));
}

point_t get_end(std::string line_str) {
    std::string end_str =
        line_str.substr(line_str.find(LINE_DELIM) + LINE_DELIM.size());
    size_t delim_pos = end_str.find(',');
    return std::make_pair(std::stoi(end_str.substr(0, delim_pos)),
                          std::stoi(end_str.substr(delim_pos + 1)));
}

line_t get_line(std::string line_str) {
    return std::make_pair(get_start(line_str), get_end(line_str));
}

bool is_diag_line(line_t l) {
    return l.second.second != l.first.second && l.second.first != l.first.first;
}

std::set<point_t> line_to_points(line_t l) {
    int x_delta =
        abs(l.first.first - l.second.first) / (l.second.first - l.first.first);
    int y_delta = abs(l.first.second - l.second.second) /
                  (l.second.second - l.first.second);
    std::set<point_t> unique_points;
    int x_curr = l.first.first;
    int y_curr = l.first.second;
    while (x_curr != l.second.first || y_curr != l.second.second) {
        unique_points.insert(std::make_pair(x_curr, y_curr));
        x_curr += x_delta;
        y_curr += y_delta;
    }
    unique_points.insert(std::make_pair(x_curr, y_curr));

    return unique_points;
}

std::set<point_t> get_intersections(line_t l1, line_t l2) {
    std::set<point_t> l1_points = line_to_points(l1);
    std::set<point_t> l2_points = line_to_points(l2);

    std::set<point_t> intersect;
    set_intersection(l1_points.begin(), l1_points.end(), l2_points.begin(),
                     l2_points.end(),
                     std::inserter(intersect, intersect.begin()));
    return intersect;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    std::vector<line_t> lines;
    std::set<point_t> intersections_with_diags;
    std::set<point_t> intersections_without_diags;
    while (getline(input_stream, line)) {
        line_t l = get_line(line);
        for (int idx = 0; idx < lines.size(); ++idx) {
            line_t cmp_line = lines[idx];
            std::set<point_t> l_intersects = get_intersections(l, cmp_line);
            intersections_with_diags.insert(l_intersects.begin(),
                                            l_intersects.end());

            if (!is_diag_line(l) && !is_diag_line(cmp_line)) {
                intersections_without_diags.insert(l_intersects.begin(),
                                                   l_intersects.end());
            }
        }
        lines.push_back(l);
    }

    std::cout << "Part 1: " << intersections_without_diags.size() << std::endl;
    std::cout << "Part 2: " << intersections_with_diags.size() << std::endl;

    return EXIT_SUCCESS;
}