#include <assert.h>

#include <fstream>
#include <iostream>
#include <regex>
#include <vector>

typedef std::vector<char> bucket_t;

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	std::vector<bucket_t> vec_buckets_1;
	std::vector<bucket_t> vec_buckets_2;

	// parse stack
	while (getline(input_stream, line) && line[1] != '1') {
		if (vec_buckets_1.empty() && vec_buckets_2.empty()) {
			// init
			for (int i = 0; i < ((line.size() - 3) / 4) + 1; i++) {
				vec_buckets_1.push_back(*(new std::vector<char>()));
				vec_buckets_2.push_back(*(new std::vector<char>()));
			}
		}

		int b_idx = 0;
		for (int i = 0; i < line.size() - 2; i += 4) {
			std::string ele_str = line.substr(i, 3);
			if (ele_str[1] > 64) {
				vec_buckets_1[b_idx].insert(vec_buckets_1[b_idx].begin(),
											ele_str[1]);
				vec_buckets_2[b_idx].insert(vec_buckets_2[b_idx].begin(),
											ele_str[1]);
			}
			b_idx++;
		}
	}

	// parse instructions
	std::regex inst_regex("move (\\d+) from (\\d+) to (\\d+)");
	getline(input_stream, line);  // ignore separator
	while (getline(input_stream, line)) {
		std::smatch matches;
		assert(std::regex_search(line, matches, inst_regex));
		assert(matches.size() == 4);

		int move_num = std::stoi(matches[1].str());
		int from_idx = std::stoi(matches[2].str()) - 1;
		int to_idx = std::stoi(matches[3].str()) - 1;

		// CrateMover 9000
		for (int i = 0; i < move_num; i++) {
			char to_move = vec_buckets_1[from_idx].back();
			vec_buckets_1[from_idx].pop_back();
			vec_buckets_1[to_idx].push_back(to_move);
		}

		// CrateMover 9001
		std::vector<char> move_block;
		for (int i = 0; i < move_num; i++) {
			char to_move = vec_buckets_2[from_idx].back();
			vec_buckets_2[from_idx].pop_back();
			move_block.insert(move_block.begin(), to_move);
		}

		for (char c : move_block) vec_buckets_2[to_idx].push_back(c);
	}

	// Part 1
	std::cout << "Part 1: ";
	for (int i = 0; i < vec_buckets_1.size(); i++)
		std::cout << vec_buckets_1[i].back();
	std::cout << std::endl;

	// Part 2
	std::cout << "Part 2: ";
	for (int i = 0; i < vec_buckets_2.size(); i++)
		std::cout << vec_buckets_2[i].back();
	std::cout << std::endl;

	return EXIT_SUCCESS;
}

