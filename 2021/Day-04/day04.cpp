#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

const int BOARD_NUM_ROWS = 5;
const int BOARD_NUM_COLS = 5;

typedef std::map<std::pair<int, int>, std::pair<int, bool>> board_pos_marked;
typedef std::map<int, std::pair<int, int>> board_val_pos;
typedef std::pair<board_pos_marked, board_val_pos> board;

void mark_board(board &b, int val) {
    board_val_pos val_pos_map = b.second;
    if (val_pos_map.find(val) != val_pos_map.end()) {
        b.first[val_pos_map[val]] = std::make_pair(val, true);
    }
}

bool check_row_win(board_pos_marked &pos_marked) {
    bool if_win = false;
    for (int r = 0; r < BOARD_NUM_ROWS; ++r) {
        bool if_win_row = true;
        for (int c = 0; c < BOARD_NUM_COLS; ++c) {
            if_win_row = if_win_row && pos_marked[std::make_pair(r, c)].second;
        }
        if_win = if_win_row || if_win;
    }

    return if_win;
}

bool check_col_win(board_pos_marked &pos_marked) {
    bool if_win = false;
    for (int c = 0; c < BOARD_NUM_COLS; ++c) {
        bool if_win_col = true;
        for (int r = 0; r < BOARD_NUM_ROWS; ++r) {
            if_win_col = if_win_col && pos_marked[std::make_pair(r, c)].second;
        }
        if_win = if_win_col || if_win;
    }

    return if_win;
}

bool if_board_win(board &b) {
    board_pos_marked pos_marked = b.first;
    return check_row_win(pos_marked) || check_col_win(pos_marked);
}

int get_sum_of_unmarked(board &b) {
    int sum = 0;
    for (int r = 0; r < BOARD_NUM_ROWS; ++r) {
        for (int c = 0; c < BOARD_NUM_COLS; ++c) {
            if (!b.first[std::make_pair(r, c)].second) {
                sum += b.first[std::make_pair(r, c)].first;
            }
        }
    }

    return sum;
}

std::vector<int> get_win_board_results(std::vector<board> &boards,
                                       std::stringstream &bingo_nums,
                                       std::vector<int> results) {
    std::string bingo_num_str;
    if (getline(bingo_nums, bingo_num_str, ',')) {
        int bingo_num = std::stoi(bingo_num_str);
        for (int b_idx; b_idx < boards.size(); ++b_idx) {
            board &b = boards[b_idx];
            mark_board(b, bingo_num);
            if (if_board_win(b)) {
                results.push_back(get_sum_of_unmarked(b) * bingo_num);
                boards.erase(boards.begin() + b_idx);
                b_idx--;
            }
        }
        return get_win_board_results(boards, bingo_nums, results);
    }

    return results;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    // get bingo numbers
    getline(input_stream, line);
    std::stringstream bingo_nums(line);

    // get boards (boards are of fixed size: 5x5)
    std::vector<board> boards;
    while (getline(input_stream, line)) {
        if (line == "") {
            std::string row;
            board_val_pos val_pos_map;
            board_pos_marked pos_marked_map;
            for (int r = 0; r < BOARD_NUM_ROWS; ++r) {
                getline(input_stream, row);
                std::stringstream row_ss(row);
                std::string col_val;
                for (int c = 0; c < BOARD_NUM_COLS; ++c) {
                    getline(row_ss, col_val, ' ');
                    // to account for double spaces
                    while (col_val == "") {
                        getline(row_ss, col_val, ' ');
                    }
                    pos_marked_map.insert(std::make_pair(
                        std::make_pair(r, c),
                        std::make_pair(std::stoi(col_val), false)));
                    val_pos_map.insert(std::make_pair(std::stoi(col_val),
                                                      std::make_pair(r, c)));
                }
            }
            boards.push_back(std::make_pair(pos_marked_map, val_pos_map));
        }
    }

    std::vector<int> results;
    std::vector<int> win_results =
        get_win_board_results(boards, bingo_nums, results);
    std::cout << "Part 1: " << win_results[0] << std::endl;
    std::cout << "Part 2: " << win_results[win_results.size() - 1] << std::endl;

    return EXIT_SUCCESS;
}
