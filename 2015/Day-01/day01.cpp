#include <fstream>
#include <iostream>

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");

	std::string line;
	getline(input_stream, line);

	long int pos = 1;
	long int floor = 0;
	long int basement_pos = -1;
	for (char c : line) {
		floor += c == '(' ? 1 : -1;
		if (floor == -1 && basement_pos == -1) {
			basement_pos = pos;
		}
		pos++;
	}

	std::cout << "Part 1: " << floor << std::endl;
	std::cout << "Part 2: " << basement_pos << std::endl;

	return EXIT_SUCCESS;
}

