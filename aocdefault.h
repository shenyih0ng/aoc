#include <vector>
#include <fstream>
#include <iterator>

template<typename T>
std::ostream& operator<< (std::ostream& out, const std::vector<T>& v) {
	if (!v.empty()) {
		out << "[";
		copy(v.begin(), v.end(), std::ostream_iterator<T>(out, ", "));
		out << "\b\b]";
	} else {
		out << "[]";
	}

	return out;
}

// helper to get data file
std::string get_input_file_path (int argc, char* argv[]);
