#include <assert.h>

#include <fstream>
#include <iostream>
#include <map>
#include <regex>

typedef std::pair<int, int> coord_t;
typedef std::map<coord_t, bool> status_grid_t;
typedef std::map<coord_t, long long int> brightness_grid_t;

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string command;

	std::regex coord_regex("(\\d+),(\\d+) through (\\d+),(\\d+)");

	status_grid_t status_grid;
	brightness_grid_t brightness_grid;
	while (getline(input_stream, command)) {
		bool toggle = command.substr(0, 6) == "toggle";
		bool off = command.substr(0, 8) == "turn off";
		bool on = !toggle && !off;

		std::smatch matches;
		std::regex_search(command, matches, coord_regex);
		assert(matches.size() == 5);

		coord_t start =
			std::make_pair(std::stoi(matches[1]), std::stoi(matches[2]));
		coord_t end =
			std::make_pair(std::stoi(matches[3]), std::stoi(matches[4]));

		for (int x = 0; x < end.first - start.first + 1; x++) {
			for (int y = 0; y < end.second - start.second + 1; y++) {
				coord_t coord =
					std::make_pair(start.first + x, start.second + y);

				if (toggle) {
					status_grid[coord] = !status_grid[coord];
					brightness_grid[coord] += 2;
				}
				if (off) {
					status_grid[coord] = false;
					brightness_grid[coord] =
						std::max((long long int)0, brightness_grid[coord] - 1);
				}
				if (on) {
					status_grid[coord] = true;
					brightness_grid[coord] += 1;
				}
			}
		}
	}

	long long int num_lit = 0, brightness_level = 0;
	for (status_grid_t::iterator s_it = status_grid.begin();
		 s_it != status_grid.end(); s_it++)
		num_lit += s_it->second;
	for (brightness_grid_t::iterator b_it = brightness_grid.begin();
		 b_it != brightness_grid.end(); b_it++)
		brightness_level += b_it->second;

	std::cout << "Part 1: " << num_lit << std::endl;
	std::cout << "Part 2: " << brightness_level << std::endl;

	return EXIT_SUCCESS;
}

