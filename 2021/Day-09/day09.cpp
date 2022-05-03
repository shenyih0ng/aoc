#include <fstream>
#include <iostream>
#include <map>
#include <vector>

typedef std::vector<std::string> grid_t;
typedef std::pair<int, int> grid_pos_t;

std::pair<int, int> operator+(const std::pair<int, int> p1,
                              const std::pair<int, int> p2) {
    return std::make_pair<int, int>(p1.first + p2.first, p1.second + p2.second);
}

bool is_pos_in_grid(grid_t &grid, grid_pos_t pos) {
    int num_rows = grid.size();
    int num_cols = grid[0].size();
    return !(pos.first < 0 || pos.first >= num_rows || pos.second < 0 ||
             pos.second >= num_cols);
}

int get_val_at_pos(grid_t &grid, grid_pos_t pos) {
    // - 48 : since ascii number characters starts at 48
    return is_pos_in_grid(grid, pos) ? grid[pos.first][pos.second] - 48 : -1;
}

// returns invalid positions as well
std::vector<grid_pos_t> get_adj_pos(grid_t &grid, grid_pos_t pos) {
    std::vector<grid_pos_t> adj_pos = {
        pos + std::make_pair(0, 1), pos + std::make_pair(0, -1),
        pos + std::make_pair(1, 0), pos + std::make_pair(-1, 0)};

    return adj_pos;
}

std::vector<int> get_adj_vals(grid_t &grid, grid_pos_t pos) {
    std::vector<grid_pos_t> adj_pos = get_adj_pos(grid, pos);
    std::vector<grid_pos_t>::iterator adjPosIt = adj_pos.begin();
    std::vector<int> adj_vals;
    for (; adjPosIt != adj_pos.end(); ++adjPosIt) {
        adj_vals.push_back(get_val_at_pos(grid, *adjPosIt));
    }

    return adj_vals;
}

bool is_lowest_pos(grid_t &grid, grid_pos_t pos) {
    int pos_val = grid[pos.first][pos.second] - 48;
    std::vector<int> adj = get_adj_vals(grid, pos);
    return std::find_if(adj.begin(), adj.end(), [pos_val, &grid](int adj_val) {
               return adj_val <= pos_val && adj_val != -1;
           }) == adj.end();
}

int get_basin_size(grid_t &grid, grid_pos_t curr_pos,
                   std::map<grid_pos_t, bool> &visited) {
    if (!is_pos_in_grid(grid, curr_pos) ||
        get_val_at_pos(grid, curr_pos) == 9) {
        return EXIT_SUCCESS;
    } else {
        int size = 0;
        int pos_val = get_val_at_pos(grid, curr_pos);
        visited[curr_pos] = true;

        std::vector<grid_pos_t> adj_pos = get_adj_pos(grid, curr_pos);
        std::vector<grid_pos_t>::iterator adjPosIt = adj_pos.begin();
        for (; adjPosIt != adj_pos.end(); ++adjPosIt) {
            if (visited.find(*adjPosIt) == visited.end()) {
                int adj_val = get_val_at_pos(grid, *adjPosIt);
                if (adj_val != -1 && adj_val >= pos_val) {
                    size += get_basin_size(grid, *adjPosIt, visited);
                }
            }
        }

        return 1 + size;
    }
}

int find_risk_level(grid_t grid) {
    int num_rows = grid.size();
    int num_cols = grid[0].size();

    int risk_level = 0;
    for (int r = 0; r < num_rows; ++r) {
        for (int c = 0; c < num_cols; ++c) {
            risk_level += is_lowest_pos(grid, std::make_pair(r, c))
                              ? grid[r][c] - 48 + 1
                              : 0;
        }
    }

    return risk_level;
}

std::vector<int> get_sorted_basin_sizes(grid_t grid) {
    int num_rows = grid.size();
    int num_cols = grid[0].size();

    std::vector<int> basin_sizes;
    for (int r = 0; r < num_rows; ++r) {
        for (int c = 0; c < num_cols; ++c) {
            grid_pos_t curr_pos = std::make_pair(r, c);
            if (is_lowest_pos(grid, curr_pos)) {
                std::map<grid_pos_t, bool> cache;
                int basin_size =
                    get_basin_size(grid, std::make_pair(r, c), cache);
                basin_sizes.push_back(basin_size);
            }
        }
    }

    std::sort(basin_sizes.begin(), basin_sizes.end());
    return basin_sizes;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    grid_t grid;
    while (getline(input_stream, line)) {
        grid.push_back(line);
    }

    std::cout << "Part 1: " << find_risk_level(grid) << std::endl;
    std::vector<int> basin_sizes = get_sorted_basin_sizes(grid);
    int num_basins = basin_sizes.size();
    std::cout << "Part 2: "
              << basin_sizes[num_basins - 1] * basin_sizes[num_basins - 2] *
                     basin_sizes[num_basins - 3]
              << std::endl;

    return EXIT_SUCCESS;
}