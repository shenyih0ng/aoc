#include <fstream>
#include <iostream>

int get_score_1(int p1, int p2) {
	if (p1 == p2) {
		// draw
		return p2 + 3;
	}

	int score = p2;
	switch (p2) {
		case 1:
			score += (p1 == 3) ? 6 : 0;
			break;
		case 2:
			score += (p1 == 1) ? 6 : 0;
			break;
		case 3:
			score += (p1 == 2) ? 6 : 0;
			break;
	}

	return score;
}

int get_score_2(int p1, int outcome) {
	if (outcome == 2) {
		// draw
		return p1 + 3;
	} else if (outcome == 1) {
		// lose
		return (p1 == 1) ? 3 : p1 - 1;
	} else {
		// win
		return (p1 == 3) ? 6 + 1 : 6 + (p1 + 1);
	}
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	long long int score_1 = 0;
	long long int score_2 = 0;
	while (getline(input_stream, line)) {
		int delimiter_idx = line.find(" ");

		// normalize
		int p1 = line.substr(0, delimiter_idx)[0] - 65 + 1;
		int p2 = line.substr(delimiter_idx + 1)[0] - 88 + 1;

		score_1 += get_score_1(p1, p2);
		score_2 += get_score_2(p1, p2);
	}

	std::cout << "Part 1: " << score_1 << std::endl;
	std::cout << "Part 2: " << score_2 << std::endl;

	return EXIT_SUCCESS;
}
