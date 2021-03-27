#include <aocdefault.h>

using namespace std;

// Assumption
// 1. The basename of the input .txt will always be the same as the source base name
// 2. Binaries will be executed in the context of the parent directory of bin dir

const std::string RUN_MODE = "run";

std::string get_input_file_path (int argc, char* argv[]) {
	std::string bin_path = argv[0];
	std::string mode;
	
	if (argc == 2) { 
		mode = argv[1]; 
	}
	// find base name
	std::string base_name;
	int idx_last_split = bin_path.find_last_of("/\\");
	base_name = bin_path.substr(idx_last_split + 1);
	int idx_ext = base_name.find_last_of(".");
	if (idx_ext > -1) {
		// remove ext if present
		base_name = base_name.substr(0, idx_ext);
	}

	if (!mode.empty() && mode == RUN_MODE) {
		return "./" + base_name + ".txt";
	}
	return "./" + base_name + "_" + mode + ".txt";
}
