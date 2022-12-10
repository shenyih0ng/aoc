#include <fstream>
#include <iostream>
#include <vector>

#define CRT_HEIGHT 6
#define CRT_WIDTH 40

void display_crt(std::vector<char> &crt) {
	for (int idx = 0; idx < CRT_HEIGHT * CRT_WIDTH; idx++) {
		if (idx % CRT_WIDTH == 0 && idx != 0) std::cout << "\n";
		std::cout << "." ? idx >= crt.size() : crt[idx];
	}
	std::cout << std::endl;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	int next_check_cycle = 20;
	int total_signal_str = 0;

	int addx_val;
	bool prev_addx = false;

	std::vector<char> crt;
	int cycle = 1, reg = 1, crt_pos = 0;
	while (true) {
		int new_reg = reg;
		if (prev_addx) {
			new_reg += addx_val;
			prev_addx = false;
		} else {
			if (!getline(input_stream, line)) break;
			std::string cmd = line.substr(0, 4);
			if (cmd == "addx") {
				addx_val = std::stoi(line.substr(5));
				prev_addx = true;
			}
		}

		if (cycle == next_check_cycle) {
			total_signal_str += (reg * cycle);
			next_check_cycle += 40;
		}

		if (crt_pos <= reg + 1 && crt_pos >= reg - 1) {
			crt.push_back('#');
		} else {
			crt.push_back('.');
		}

		reg = new_reg;
		cycle++;
		crt_pos = (crt_pos + 1) % CRT_WIDTH;
	}

	std::cout << "Part 1: " << total_signal_str << std::endl;
	std::cout << "Part 2:" << std::endl;
	display_crt(crt);

	return EXIT_SUCCESS;
}

