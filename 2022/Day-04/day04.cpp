#include <fstream>
#include <iostream>

typedef std::pair<int, int> section_t;

section_t get_section(std::string sec_str) {
	int delimiter_idx = sec_str.find('-');
	return std::make_pair(std::stoi(sec_str.substr(0, delimiter_idx)),
						  std::stoi(sec_str.substr(delimiter_idx + 1)));
}

bool contained(section_t s1, section_t s2) {
	return (s1.first >= s2.first && s1.second <= s2.second) ||
		   (s2.first >= s1.first && s2.second <= s1.second);
}

bool overlap(section_t s1, section_t s2) {
	return std::max(s1.first, s2.first) <= std::min(s1.second, s2.second);
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	long int num_overlapped = 0;
	long int num_contained = 0;
	while (getline(input_stream, line)) {
		int delimiter_idx = line.find(',');
		std::string p1 = line.substr(0, delimiter_idx);
		std::string p2 = line.substr(delimiter_idx + 1);

		section_t s1 = get_section(p1);
		section_t s2 = get_section(p2);

		if (contained(s1, s2)) {
			num_contained += 1;
			num_overlapped += 1;
		} else if (overlap(s1, s2)) {
			num_overlapped += 1;
		}
	}

	std::cout << "Part 1: " << num_contained << std::endl;
	std::cout << "Part 2: " << num_overlapped << std::endl;

	return EXIT_SUCCESS;
}

