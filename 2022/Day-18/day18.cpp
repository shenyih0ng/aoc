#include <assert.h>

#include <fstream>
#include <iostream>
#include <queue>
#include <regex>
#include <set>
#include <vector>

typedef std::tuple<int, int, int> coord_tupl_t;

std::regex coord_regex("(\\d+),(\\d+),(\\d+)");

struct Cube {
	int x, y, z;
	bool covered[6];  // top, btm, left, right, front, back
};

void cmp(Cube& c1, Cube& c2) {
	bool matched_axis[3] = {
		c1.x == c2.x,
		c1.y == c2.y,
		c1.z == c2.z,
	};
	int num_matched_axis = matched_axis[0] + matched_axis[1] + matched_axis[2];

	if (num_matched_axis < 2) return;

	if (std::abs(c1.x - c2.x) == 1) {
		c1.covered[c1.x < c2.x ? 3 : 2] = true;
		c2.covered[c1.x < c2.x ? 2 : 3] = true;
	} else if (std::abs(c1.y - c2.y) == 1) {
		c1.covered[c1.y < c2.y ? 0 : 1] = true;
		c2.covered[c1.y < c2.y ? 1 : 0] = true;
	} else if (std::abs(c1.z - c2.z) == 1) {
		c1.covered[c1.z < c2.z ? 5 : 4] = true;
		c2.covered[c1.z < c2.z ? 4 : 5] = true;
	}
}

int get_num_covered(Cube& c) {
	int num_covered = 0;
	for (bool is_covered : c.covered) num_covered += is_covered;
	return num_covered;
}

bool contains(std::vector<Cube>& cubes, int x, int y, int z) {
	for (Cube& cube : cubes) {
		if (cube.x == x && cube.y == y && cube.z == z) return true;
	}

	return false;
}

std::vector<coord_tupl_t> get_neighbours(coord_tupl_t coord) {
	int x, y, z;
	std::tie(x, y, z) = coord;
	return {{x - 1, y, z}, {x + 1, y, z}, {x, y - 1, z},
			{x, y + 1, z}, {x, y, z - 1}, {x, y, z + 1}};
}

int get_ext_area(std::vector<Cube>& cubes) {
	int min_x = 99, max_x = -1, min_y = 99, max_y = -1, min_z = 99, max_z = -1;
	for (Cube& cube : cubes) {
		min_x = std::min(min_x, cube.x - 1);
		max_x = std::max(max_x, cube.x + 1);
		min_y = std::min(min_y, cube.y - 1);
		max_y = std::max(max_y, cube.y + 1);
		min_z = std::min(min_z, cube.z - 1);
		max_z = std::max(max_z, cube.z + 1);
	}

	int area = 0;
	std::set<coord_tupl_t> seen;
	std::queue<coord_tupl_t> queue;

	int x = min_x, y = min_y, z = min_z;
	queue.push(std::make_tuple(x, y, z));
	seen.insert(std::make_tuple(x, y, z));
	while (!queue.empty()) {
		coord_tupl_t coord_tupl = queue.front();
		queue.pop();

		std::vector<coord_tupl_t> adjs = get_neighbours(coord_tupl);
		for (coord_tupl_t adj : adjs) {
			int a_x, a_y, a_z;
			std::tie(a_x, a_y, a_z) = adj;

			if (a_x < min_x || a_x > max_x || a_y < min_y || a_y > max_y ||
				a_z < min_z || a_z > max_z)	 // out of bounds
				continue;
			if (seen.find(adj) != seen.end()) continue;	 // already visited
			if (contains(cubes, a_x, a_y, a_z)) {
				area += 1;
				continue;  // need to consider the different faces
			}

			seen.insert(adj);
			queue.push(adj);
		}
	}

	return area;
}

int main(int argc, char* argv[]) {
	std::ifstream input_stream("./input.txt");

	std::string coord;	// all coords are unique
	std::vector<Cube> cubes;
	while (getline(input_stream, coord)) {
		std::smatch matches;
		std::regex_search(coord, matches, coord_regex);
		assert(matches.size() == 4);

		Cube curr_cube = {std::stoi(matches[1]), std::stoi(matches[2]),
						  std::stoi(matches[3])};
		for (Cube& cube : cubes) cmp(curr_cube, cube);
		cubes.push_back(curr_cube);
	}

	int total_covered = 0;
	for (Cube& cube : cubes) total_covered += get_num_covered(cube);
	int total_uncovered = cubes.size() * 6 - total_covered;

	std::cout << "Part 1: " << total_uncovered << std::endl;
	std::cout << "Part 2: " << get_ext_area(cubes) << std::endl;

	return EXIT_SUCCESS;
}

