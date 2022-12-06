#include <fstream>
#include <iostream>
#include <set>

int find_pos_of_unique_substr(std::string data_stream, int substr_len) {
	int unique_substr_pos = 0;	// 1-indexed
	for (int idx = 0; idx < data_stream.size() - substr_len + 1; idx++) {
		std::set<char> c_set;
		std::string check = data_stream.substr(idx, substr_len);
		for (char c : check) c_set.insert(c);

		if (c_set.size() == check.size()) {
			unique_substr_pos = idx + substr_len;
			break;
		}
	}

	return unique_substr_pos;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");

	std::string data_stream;
	std::getline(input_stream, data_stream);

	std::cout << "Part 1: " << find_pos_of_unique_substr(data_stream, 4)
			  << std::endl;
	std::cout << "Part 2: " << find_pos_of_unique_substr(data_stream, 14)
			  << std::endl;

	return EXIT_SUCCESS;
}

