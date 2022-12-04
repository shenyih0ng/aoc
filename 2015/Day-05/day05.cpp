#include <fstream>
#include <iostream>
#include <map>

int get_num_vowels(std::string str) {
	// https://stackoverflow.com/a/47846874
	int num_vowels = 0;
	for (char c : str) num_vowels += (0x208222 >> (c & 0x1f)) & 1;
	return num_vowels;
}

bool is_nice_str_1(std::string str) {
	int num_vowels = get_num_vowels(str);
	if (num_vowels < 3) return false;

	bool has_duplicate = false;
	for (int idx = 0; idx < str.size() - 1; idx++) {
		std::string pair = str.substr(idx, 2);
		has_duplicate = has_duplicate || pair[0] == pair[1];
		if (pair == "ab" || pair == "cd" || pair == "pq" || pair == "xy") {
			return false;
		}
	}

	return has_duplicate;
}

bool is_nice_str_2(std::string str) {
	bool has_pair = false;
	bool has_repeat = false;

	std::string prev = "";
	std::map<std::string, int> dup_map;
	for (int idx = 0; idx < str.size() - 1; idx++) {
		std::string pair = str.substr(idx, 2);
		if (dup_map.find(pair) == dup_map.end()) {
			dup_map[pair] = 1;
		} else if (pair != prev) {
			// >= 2
			has_pair = true;
			break;
		}
		prev = pair;
	}

	for (int idx = 0; idx < str.size() - 2; idx++) {
		std::string tripl = str.substr(idx, 3);
		if (tripl[0] == tripl[2]) {
			has_repeat = true;
			break;
		}
	}

	return has_pair && has_repeat;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	long int num_nice_strs_1 = 0, num_nice_strs_2 = 0;
	while (getline(input_stream, line)) {
		num_nice_strs_1 += is_nice_str_1(line);
		num_nice_strs_2 += is_nice_str_2(line);
	}

	std::cout << "Part 1: " << num_nice_strs_1 << std::endl;
	std::cout << "Part 2: " << num_nice_strs_2 << std::endl;

	return EXIT_SUCCESS;
}

