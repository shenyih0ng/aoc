#include <fstream>
#include <iostream>

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	while (getline(input_stream, line)) {
		std::cout << line << std::endl;
	}

	return EXIT_SUCCESS;
}

