#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

typedef std::vector<int> grid_row_t;
typedef std::vector<grid_row_t> grid_t;
typedef std::pair<int, int> grid_pos_t;

std::ostream &operator<<(std::ostream &out, grid_row_t &r) {
    std::copy(r.begin(), r.end(), std::ostream_iterator<int>(out, " "));
    return out;
}

std::ostream &operator<<(std::ostream &out, grid_t &g) {
    grid_t::iterator gIt = g.begin();
    for (; gIt != g.end(); ++gIt) {
        out << *gIt << std::endl;
    }

    return out;
}

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
    return is_pos_in_grid(grid, pos) ? grid[pos.first][pos.second] : -1;
}

// returns invalid positions as well
std::vector<grid_pos_t> get_adj_pos(grid_t &grid, grid_pos_t pos) {
    std::vector<grid_pos_t> adj_pos = {
        pos + std::make_pair(0, 1),   pos + std::make_pair(0, -1),
        pos + std::make_pair(1, 0),   pos + std::make_pair(-1, 0),
        pos + std::make_pair(1, 1),   pos + std::make_pair(1, -1),
        pos + std::make_pair(-1, -1), pos + std::make_pair(-1, 1)};

    return adj_pos;
}

std::pair<grid_t, int> step(grid_t &grid) {
    grid_t next_grid = grid;
    int order = grid.size();

    // add 1
    for (int r = 0; r < order; ++r) {
        for (int c = 0; c < order; ++c) {
            next_grid[r][c] = grid[r][c] + 1;
        }
    }

    // flash
    std::map<grid_pos_t, bool> flashed;
    bool done = false;
    while (!done) {
        done = true;
        for (int r = 0; r < order; ++r) {
            for (int c = 0; c < order; ++c) {
                grid_pos_t pos = std::make_pair(r, c);
                if (next_grid[r][c] > 9 && flashed.find(pos) == flashed.end()) {
                    std::vector<grid_pos_t> adjs = get_adj_pos(next_grid, pos);
                    std::vector<grid_pos_t>::iterator adjIt = adjs.begin();
                    for (; adjIt != adjs.end(); ++adjIt) {
                        grid_pos_t adj_pos = *adjIt;
                        if (is_pos_in_grid(next_grid, adj_pos)) {
                            next_grid[adj_pos.first][adj_pos.second] += 1;
                        }
                    }
                    flashed[pos] = true;
                    done = false;
                }
            }
        }
    }

    // reset
    for (int r = 0; r < order; ++r) {
        for (int c = 0; c < order; ++c) {
            if (next_grid[r][c] > 9) {
                next_grid[r][c] = 0;
            }
        }
    }

    return make_pair(next_grid, flashed.size());
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    grid_t grid;
    while (getline(input_stream, line)) {
        grid_row_t row;
        std::string::iterator lineIt = line.begin();
        for (; lineIt != line.end(); ++lineIt) {
            row.push_back(*lineIt - 48);
        }
        grid.push_back(row);
    }

    int all_flash_step_count = -1;
    int step_count = 0;
    int total_flashes_100_steps = 0;
    while (all_flash_step_count == -1 || step_count < 100) {
        std::pair<grid_t, int> next_state = step(grid);
        grid = next_state.first;
        total_flashes_100_steps += step_count < 100 ? next_state.second : 0;
        if (next_state.second == 100 && all_flash_step_count == -1) {
            all_flash_step_count = step_count + 1;
        }
        step_count++;
    }

    std::cout << "Part 1: " << total_flashes_100_steps << std::endl;
    std::cout << "Part 2: " << all_flash_step_count << std::endl;

    return EXIT_SUCCESS;
}