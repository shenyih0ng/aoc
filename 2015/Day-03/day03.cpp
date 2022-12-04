#include <fstream>
#include <iostream>
#include <map>

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string directions;
	getline(input_stream, directions);

	int x = 0, y = 0;
	int s_x = 0, s_y = 0;
	int r_x = 0, r_y = 0;
	std::map<std::pair<int, int>, bool> cache_1, cache_2;
	cache_1.insert(std::make_pair(std::make_pair(0, 0), true));
	cache_2.insert(std::make_pair(std::make_pair(0, 0), true));

	bool is_santa = true;
	for (char dir : directions) {
		if (is_santa) {
			s_x += (dir == '<') ? -1 : (dir == '>') ? 1 : 0;
			s_y += (dir == 'v') ? -1 : (dir == '^') ? 1 : 0;
			cache_2.insert(std::make_pair(std::make_pair(s_x, s_y), true));
		} else {
			r_x += (dir == '<') ? -1 : (dir == '>') ? 1 : 0;
			r_y += (dir == 'v') ? -1 : (dir == '^') ? 1 : 0;
			cache_2.insert(std::make_pair(std::make_pair(r_x, r_y), true));
		}
		is_santa = !is_santa;

		x += (dir == '<') ? -1 : (dir == '>') ? 1 : 0;
		y += (dir == 'v') ? -1 : (dir == '^') ? 1 : 0;
		cache_1.insert(std::make_pair(std::make_pair(x, y), true));
	}

	std::cout << "Part 1: " << cache_1.size() << std::endl;
	std::cout << "Part 2: " << cache_2.size() << std::endl;

	return EXIT_SUCCESS;
}

