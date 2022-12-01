#include <fstream>
#include <iostream>
#include <vector>

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	long long int curr = 0;
	std::vector<long long int> calories;
	while (getline(input_stream, line)) {
		if (line == "") {
			calories.push_back(curr);
			curr = 0;
			continue;
		}

		long long int calorie = std::stoi(line);
		curr += calorie;
	}
	std::sort(calories.begin(), calories.end());

	std::cout << "Part 1: " << calories[calories.size() - 1] << std::endl;
	std::cout << "Part 2: "
			  << calories[calories.size() - 1] + calories[calories.size() - 2] +
					 calories[calories.size() - 3]
			  << std::endl;

	return EXIT_SUCCESS;
}
