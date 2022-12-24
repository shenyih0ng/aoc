#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <vector>

#define ROUNDS 10

typedef int64_t ll;
typedef std::pair<ll, ll> coord_t;

std::vector<coord_t> deltas = {{-1, -1}, {-1, 0}, {-1, 1}, {1, -1},
							   {1, 0},	 {1, 1},  {0, -1}, {0, 1}};

std::vector<coord_t> get_adj(coord_t &coord) {
	std::vector<coord_t> adjs;
	for (coord_t d : deltas)
		adjs.push_back(
			std::make_pair(coord.first + d.first, coord.second + d.second));

	return adjs;
}

std::vector<coord_t> get_north_adj(coord_t &coord) {
	return {{coord.first - 1, coord.second - 1},
			{coord.first - 1, coord.second},
			{coord.first - 1, coord.second + 1}};
}

std::vector<coord_t> get_south_adj(coord_t &coord) {
	return {{coord.first + 1, coord.second - 1},
			{coord.first + 1, coord.second},
			{coord.first + 1, coord.second + 1}};
}

std::vector<coord_t> get_west_adj(coord_t &coord) {
	return {{coord.first - 1, coord.second - 1},
			{coord.first, coord.second - 1},
			{coord.first + 1, coord.second - 1}};
}

std::vector<coord_t> get_east_adj(coord_t &coord) {
	return {{coord.first - 1, coord.second + 1},
			{coord.first, coord.second + 1},
			{coord.first + 1, coord.second + 1}};
}

int num_coord_occupied(std::vector<coord_t> coords,
					   std::set<coord_t> &occupied) {
	int num_occupied = 0;
	for (coord_t c : coords) num_occupied += occupied.find(c) != occupied.end();
	return num_occupied;
}

std::vector<coord_t> get_proposed_dir_adjs(coord_t &coord, int proposed_dir) {
	switch (proposed_dir) {
		case 0:
			return get_north_adj(coord);
		case 1:
			return get_south_adj(coord);
		case 2:
			return get_west_adj(coord);
		case 3:
			return get_east_adj(coord);
	}

	return {};
}

void simulate(std::set<coord_t> &occupied, int proposed_dir) {
	std::map<coord_t, int> proposed;
	std::map<coord_t, coord_t> move_map;

	std::set<coord_t>::iterator it = occupied.begin();
	for (; it != occupied.end(); it++) {
		coord_t curr_coord = *it;
		if (num_coord_occupied(get_adj(curr_coord), occupied) == 0) continue;

		int count = 0;
		int p_dir = proposed_dir;
		while (num_coord_occupied(get_proposed_dir_adjs(curr_coord, p_dir),
								  occupied) > 0 &&
			   count < 4) {
			p_dir = (p_dir + 1) % 4;
			count++;
		}
		if (count == 4) continue;

		coord_t proposed_coord;
		switch (p_dir) {
			case 0:
				proposed_coord =
					std::make_pair(curr_coord.first - 1, curr_coord.second);
				break;
			case 1:
				proposed_coord =
					std::make_pair(curr_coord.first + 1, curr_coord.second);
				break;
			case 2:
				proposed_coord =
					std::make_pair(curr_coord.first, curr_coord.second - 1);
				break;
			case 3:
				proposed_coord =
					std::make_pair(curr_coord.first, curr_coord.second + 1);
				break;
		}

		proposed[proposed_coord] += 1;
		move_map[curr_coord] = proposed_coord;
	}

	std::map<coord_t, coord_t>::iterator move_it = move_map.begin();
	for (; move_it != move_map.end(); move_it++) {
		if (proposed[(*move_it).second] > 1) continue;

		occupied.erase((*move_it).first);
		occupied.insert((*move_it).second);
	}
}

ll get_num_empty(std::set<coord_t> &occupied) {
	ll min_x = std::numeric_limits<ll>::max(),
	   min_y = std::numeric_limits<ll>::max(),
	   max_x = std::numeric_limits<ll>::min(),
	   max_y = std::numeric_limits<ll>::min();

	for (coord_t c : occupied) {
		min_x = std::min(c.first, min_x);
		min_y = std::min(c.second, min_y);
		max_x = std::max(c.first, max_x);
		max_y = std::max(c.second, max_y);
	}

	return (max_x - min_x + 1) * (max_y - min_y + 1) - occupied.size();
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	std::set<coord_t> occupied;
	ll row = 0;
	while (getline(input_stream, line)) {
		ll col = 0;
		for (char c : line) {
			if (c == '#') occupied.insert(std::make_pair(row, col));
			col++;
		}
		row++;
	}

	int dir = 0;
	for (int _i = 0; _i < ROUNDS; _i++) {
		simulate(occupied, dir);
		dir = (dir + 1) % 4;
	}

	std::cout << "Part 1: " << get_num_empty(occupied) << std::endl;

	int curr_round = ROUNDS + 1;
	while (true) {
		std::set<coord_t> occupied_copy = occupied;
		simulate(occupied, dir);
		if (occupied == occupied_copy) break;
		dir = (dir + 1) % 4;
		curr_round++;
	}

	std::cout << "Part 2: " << curr_round << std::endl;

	return EXIT_SUCCESS;
}

