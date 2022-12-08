#include <fstream>
#include <iostream>
#include <vector>

typedef std::vector<int> grid_t;

bool is_valid_pos(int rows, int cols, int r_idx, int c_idx) {
	return !(r_idx < 0 || r_idx >= rows || c_idx < 0 || c_idx >= cols);
}

bool is_visible_helper(grid_t &grid, int rows, int cols, int r_idx, int c_idx,
					   int r_delta, int c_delta) {
	int height = grid[r_idx * rows + c_idx];
	int curr_r = r_idx + r_delta, curr_c = c_idx + c_delta;

	while (is_valid_pos(rows, cols, curr_r, curr_c)) {
		if (grid[curr_r * rows + curr_c] >= height) return false;
		curr_r += r_delta;
		curr_c += c_delta;
	}

	return true;
}

bool is_visible(grid_t &grid, int rows, int cols, int r_idx, int c_idx) {
	return is_visible_helper(grid, rows, cols, r_idx, c_idx, 0, -1) ||	// left
		   is_visible_helper(grid, rows, cols, r_idx, c_idx, 0, 1) ||	// right
		   is_visible_helper(grid, rows, cols, r_idx, c_idx, 1, 0) ||	// down
		   is_visible_helper(grid, rows, cols, r_idx, c_idx, -1, 0);	// up
}

int get_num_visible_trees(grid_t &grid, int rows, int cols) {
	int inner_visible = 0;
	for (int r_idx = 1; r_idx < rows - 1; r_idx++) {
		for (int c_idx = 1; c_idx < cols - 1; c_idx++)
			inner_visible += is_visible(grid, rows, cols, r_idx, c_idx);
	}

	int edges = 2 * rows + 2 * cols - 4;
	return edges + inner_visible;
}

int scenic_score_helper(grid_t &grid, int rows, int cols, int r_idx, int c_idx,
						int r_delta, int c_delta) {
	int height = grid[r_idx * rows + c_idx];
	int curr_r = r_idx + r_delta, curr_c = c_idx + c_delta;

	int partial_score = 0;
	while (is_valid_pos(rows, cols, curr_r, curr_c)) {
		partial_score++;
		if (grid[curr_r * rows + curr_c] >= height) break;
		curr_r += r_delta;
		curr_c += c_delta;
	}

	return partial_score;
}

int get_scenic_score(grid_t &grid, int rows, int cols, int r_idx, int c_idx) {
	return scenic_score_helper(grid, rows, cols, r_idx, c_idx, 0, -1) *
		   scenic_score_helper(grid, rows, cols, r_idx, c_idx, 0, 1) *
		   scenic_score_helper(grid, rows, cols, r_idx, c_idx, 1, 0) *
		   scenic_score_helper(grid, rows, cols, r_idx, c_idx, -1, 0);
}

int get_max_scenic_score(grid_t &grid, int rows, int cols) {
	int max_score = -1;
	for (int r_idx = 1; r_idx < rows - 1; r_idx++) {
		for (int c_idx = 1; c_idx < cols - 1; c_idx++)
			max_score = std::max(
				get_scenic_score(grid, rows, cols, r_idx, c_idx), max_score);
	}

	return max_score;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	int rows = 0, cols = 0;
	std::vector<int> grid;
	while (getline(input_stream, line)) {
		rows = line.size();
		cols++;
		for (char c : line) grid.push_back(c);
	}

	std::cout << "Part 1: " << get_num_visible_trees(grid, rows, cols)
			  << std::endl;
	std::cout << "Part 2: " << get_max_scenic_score(grid, rows, cols)
			  << std::endl;

	return EXIT_SUCCESS;
}

