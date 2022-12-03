#include <assert.h>

#include <fstream>
#include <iostream>
#include <iterator>
#include <set>
#include <vector>

int get_priority(char c) {
	// a-z: 1-26
	// A-Z: 27-52
	return ((int)c >= 97) ? c - 96 : c - 38;
}

std::set<char> str_to_set(std::string str) {
	std::set<char> c_set;
	for (char c : str) c_set.insert(c);
	return c_set;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string rucksack;

	long int priority_1 = 0;
	long int priority_2 = 0;

	int count = 0;
	std::set<char> g_common;
	while (getline(input_stream, rucksack)) {
		int partition_idx = rucksack.size() / 2;
		std::set<char> c_set_1 = str_to_set(rucksack.substr(0, partition_idx));
		std::set<char> c_set_2 = str_to_set(rucksack.substr(partition_idx));

		std::vector<char> c_common;
		set_intersection(c_set_1.begin(), c_set_1.end(), c_set_2.begin(),
						 c_set_2.end(), std::back_inserter(c_common));

		assert(c_common.size() == 1);
		priority_1 += get_priority(c_common[0]);

		std::set<char> g_set = str_to_set(rucksack);
		if (count % 3 == 0) {
			priority_2 +=
				g_common.size() > 0 ? get_priority(*g_common.begin()) : 0;
			g_common = g_set;
		} else {
			std::set<char> g_new_common;
			set_intersection(g_set.begin(), g_set.end(), g_common.begin(),
							 g_common.end(),
							 std::inserter(g_new_common, g_new_common.begin()));
			g_common = g_new_common;
		}

		count++;
	}

	assert(g_common.size() == 1);
	priority_2 += get_priority(*g_common.begin());

	std::cout << "Part 1: " << priority_1 << std::endl;
	std::cout << "Part 2: " << priority_2 << std::endl;

	return EXIT_SUCCESS;
}
