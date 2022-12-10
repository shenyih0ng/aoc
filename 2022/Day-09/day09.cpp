#include <fstream>
#include <iostream>
#include <set>
#include <vector>

typedef std::pair<int, int> coord_t;
// D, L, R, U
const coord_t deltas[4] = {{0, -1}, {-1, 0}, {1, 0}, {0, 1}};

int get_unit(int val) { return val / std::abs(val); }

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	std::vector<coord_t> knots;
	for (int _i = 0; _i < 10; _i++) knots.push_back(std::make_pair(0, 0));

	std::set<coord_t> visited_p1{{0, 0}}, visited_p2{{0, 0}};

	while (getline(input_stream, line)) {
		int dir = line[0];
		int dir_count = std::stoi(line.substr(2));

		coord_t delta;
		switch (dir) {
			case 'D':
				delta = deltas[0];
				break;
			case 'L':
				delta = deltas[1];
				break;
			case 'R':
				delta = deltas[2];
				break;
			case 'U':
				delta = deltas[3];
				break;
		}

		for (int _c = 0; _c < dir_count; _c++) {
			coord_t head = knots[0];
			coord_t new_head = std::make_pair(head.first + delta.first,
											  head.second + delta.second);
			knots[0] = new_head;

			for (int k_idx = 1; k_idx < knots.size(); k_idx++) {
				coord_t curr_tail = knots[k_idx];
				coord_t curr_head = knots[k_idx - 1];
				coord_t delta =
					std::make_pair(curr_head.first - curr_tail.first,
								   curr_head.second - curr_tail.second);

				if (std::abs(delta.first) >= 2 || std::abs(delta.second) >= 2) {
					knots[k_idx] = std::make_pair(
						get_unit(delta.first) + curr_tail.first,
						get_unit(delta.second) + curr_tail.second);
				}

				if (k_idx == 1) visited_p1.insert(knots[k_idx]);
				if (k_idx == 9) visited_p2.insert(knots[k_idx]);
			}
		}
	}

	std::cout << "Part 1: " << visited_p1.size() << std::endl;
	std::cout << "Part 2: " << visited_p2.size() << std::endl;

	return EXIT_SUCCESS;
}

