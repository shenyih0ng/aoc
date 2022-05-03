#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <vector>

typedef std::string grid_row_t;
typedef std::vector<grid_row_t> grid_t;
typedef std::pair<int, int> grid_pos_t;
typedef std::pair<grid_pos_t, int> grid_pos_dist_t;

std::ostream &operator<<(std::ostream &out, grid_pos_t &r) {
    out << r.first << " " << r.second << std::endl;
    return out;
}

std::pair<int, int> operator+(const std::pair<int, int> p1,
                              const std::pair<int, int> p2) {
    return std::make_pair<int, int>(p1.first + p2.first, p1.second + p2.second);
}

std::pair<int, int> operator-(const std::pair<int, int> p1,
                              const std::pair<int, int> p2) {
    return std::make_pair<int, int>(p1.first - p2.first, p1.second - p2.second);
}

bool is_pos_in_grid(int num_rows, int num_cols, grid_pos_t pos) {
    return !(pos.first < 0 || pos.first >= num_rows || pos.second < 0 ||
             pos.second >= num_cols);
}

int get_val_at_pos(grid_t &grid, int num_rows, int num_cols, grid_pos_t pos) {
    grid_pos_t og_grid_pos =
        std::make_pair(pos.first % num_rows, pos.second % num_cols);
    grid_pos_t pos_diff = pos - og_grid_pos;
    grid_pos_t pos_diff_mod =
        std::make_pair(pos_diff.first / num_rows, pos_diff.second / num_cols);
    int pos_val = grid[og_grid_pos.first][og_grid_pos.second] - 48 +
                  pos_diff_mod.first + pos_diff_mod.second;
    if (pos_val > 9) {
        pos_val = pos_val % 9;
    }

    return pos_val;
}

// We need to include all 4 directions
std::vector<grid_pos_t> get_adj_pos(grid_t &grid, grid_pos_t pos) {
    std::vector<grid_pos_t> adj_pos = {
        pos + std::make_pair(0, 1), pos + std::make_pair(1, 0),
        pos + std::make_pair(-1, 0), pos + std::make_pair(0, -1)};

    return adj_pos;
}

struct GreaterPosDist {
    bool operator()(grid_pos_dist_t &a, grid_pos_dist_t &b) {
        return a.second > b.second;
    }
};

/*
Consider the case here:
    111
    991
    111
    199
    111

This is the reason why we would want to get the position with the least dist
from source on every iteration of the loop.
*/
int find_least_risky_path(grid_t &grid, int scale_factor = 1) {
    int num_rows = grid.size();
    int num_cols = grid[0].size();
    grid_pos_t start = std::make_pair(0, 0);
    grid_pos_t end = std::make_pair(num_rows * scale_factor - 1,
                                    num_cols * scale_factor - 1);

    std::priority_queue<grid_pos_dist_t, std::vector<grid_pos_dist_t>,
                        GreaterPosDist>
        pq;
    std::map<grid_pos_t, int> dist;
    std::set<grid_pos_t> visited;
    // initialization
    for (int r = 0; r < num_rows * scale_factor; ++r) {
        for (int c = 0; c < num_cols * scale_factor; ++c) {
            grid_pos_t pos = std::make_pair(r, c);
            dist[pos] = INT_MAX;
        }
    }
    dist[start] = 0;
    pq.push(std::make_pair(start, 0));

    bool reached_end = false;
    while (!pq.empty() && !reached_end) {
        grid_pos_t min = pq.top().first;
        pq.pop();
        visited.insert(min);

        if (min == end) {
            reached_end = true;
            continue;
        }

        std::vector<grid_pos_t> adjs = get_adj_pos(grid, min);
        std::vector<grid_pos_t>::iterator adjIt = adjs.begin();
        for (; adjIt != adjs.end(); ++adjIt) {
            if (is_pos_in_grid(num_rows * scale_factor, num_cols * scale_factor,
                               *adjIt) &&
                visited.find(*adjIt) == visited.end()) {
                // position val
                int pos_val = get_val_at_pos(grid, num_rows, num_cols, *adjIt);
                int total_val = dist[min] + pos_val;
                if (total_val < dist[*adjIt]) {
                    dist[*adjIt] = total_val;
                    pq.push(std::make_pair(*adjIt, dist[*adjIt]));
                }
            }
        }
    }

    return dist[end];
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    grid_t grid;
    while (getline(input_stream, line)) {
        grid.push_back(line);
    }

    std::cout << "Part 1: " << find_least_risky_path(grid, 1) << std::endl;
    std::cout << "Part 2: " << find_least_risky_path(grid, 5) << std::endl;

    return EXIT_SUCCESS;
}