#include <fstream>
#include <iostream>
#include <sstream>

long int get_paper(int l, int w, int h) {
	return (2 * l * w) + (2 * l * h) + (2 * w * h) +
		   std::min(std::min(l * w, l * h), w * h);
}

long int get_ribbon(int l, int w, int h) {
	int ribbon = l * w * h;

	int smallest_side = std::min(l, std::min(w, h));
	if (smallest_side == l) {
		ribbon += std::min(w, h) * 2 + smallest_side * 2;
	} else if (smallest_side == w) {
		ribbon += std::min(l, h) * 2 + smallest_side * 2;
	} else {
		ribbon += std::min(l, w) * 2 + smallest_side * 2;
	}

	return ribbon;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	long int total_paper = 0;
	long int total_ribbon = 0;
	while (getline(input_stream, line)) {
		std::istringstream ss(line);
		std::string l, w, h;
		std::getline(ss, l, 'x');
		std::getline(ss, w, 'x');
		std::getline(ss, h, 'x');

		total_paper += get_paper(std::stoi(l), std::stoi(w), std::stoi(h));
		total_ribbon += get_ribbon(std::stoi(l), std::stoi(w), std::stoi(h));
	}

	std::cout << "Part 1: " << total_paper << std::endl;
	std::cout << "Part 2: " << total_ribbon << std::endl;

	return EXIT_SUCCESS;
}

