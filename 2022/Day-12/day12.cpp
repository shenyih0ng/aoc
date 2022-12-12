#include <fstream>
#include <iostream>
#include <queue>
#include <set>
#include <vector>

int NUM_ROWS = 0, NUM_COLS = 0;

typedef std::pair<int, int> coord_t;
typedef std::pair<int, coord_t> dist_coord_t;

const coord_t DELTAS[4] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

bool is_valid_coord(coord_t &coord) {
	return coord.first >= 0 && coord.first < NUM_ROWS && coord.second >= 0 &&
		   coord.second < NUM_COLS;
}

bool can_move(coord_t &from, coord_t &to, std::vector<char> &grid) {
	char to_char = grid[to.first * NUM_COLS + to.second];
	char from_char = grid[from.first * NUM_COLS + from.second];
	return (to_char == 'E' ? 'z' : to_char) -
			   (from_char == 'S' ? 'a' : from_char) <=
		   1;
}

int bfs(std::vector<coord_t> starts, coord_t end, std::vector<char> &grid) {
	std::queue<dist_coord_t> queue;
	std::set<coord_t> visited;

	for (coord_t start : starts) {
		queue.push(std::make_pair(0, start));
		visited.insert(start);
	}

	while (!queue.empty()) {
		dist_coord_t curr = queue.front();
		queue.pop();

		coord_t curr_coord = curr.second;
		for (coord_t delta : DELTAS) {
			coord_t new_coord =
				std::make_pair(curr_coord.first + delta.first,
							   curr_coord.second + delta.second);
			if (is_valid_coord(new_coord) &&
				can_move(curr_coord, new_coord, grid) &&
				visited.find(new_coord) == visited.end()) {
				if (new_coord == end) return curr.first + 1;

				queue.push(std::make_pair(curr.first + 1, new_coord));
				visited.insert(new_coord);
			}
		}
	}

	return -1;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	coord_t start, end;
	std::vector<coord_t> starts;
	std::vector<char> grid;
	while (getline(input_stream, line)) {
		for (int i = 0; i < line.size(); i++) {
			char c = line[i];
			if (c == 'S') {
				start = std::make_pair(NUM_ROWS, i);
				starts.push_back(std::make_pair(NUM_ROWS, i));
			}
			if (c == 'E') end = std::make_pair(NUM_ROWS, i);
			if (c == 'a') starts.push_back(std::make_pair(NUM_ROWS, i));
			grid.push_back(c);
		}
		NUM_COLS = line.size();
		NUM_ROWS++;
	}

	std::cout << "Part 1: " << bfs({start}, end, grid) << std::endl;
	std::cout << "Part 2: " << bfs(starts, end, grid) << std::endl;

	return EXIT_SUCCESS;
}

