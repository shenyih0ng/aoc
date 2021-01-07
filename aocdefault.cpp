#include <iostream>

using namespace std;

// Assumption
// 1. The basename of the input .txt will always be the same as the source base name
// 2. Execution context will always be in the ~/Desktop/adventofcode/2020 root folder

std::string get_input_file_path (char* argv[]) {
	std::string bin_path = argv[0];

	int idx_last_split = bin_path.find_last_of("/\\");
	std::string base_name = bin_path.substr(idx_last_split + 1);
	int idx_ext = base_name.find_last_of(".");
	if (idx_ext > -1) {
		// ext present
		base_name = base_name.substr(0, idx_ext);
	}

	return "./" + base_name + ".txt";
}
