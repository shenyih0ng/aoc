#include <fstream>
#include <iostream>

typedef long long int ll;

ll diff(std::string str) {
	ll c_count = 0, s_count = 0;
	for (int i = 0; i < str.size(); i++) {
		char c = str[i];
		bool is_esc = c == '\\';
		if (!is_esc) {
			s_count++;
			c_count++;
			continue;
		}

		char c1 = str[i + 1];
		if (c1 == '"' || c1 == '\\') {
			i++;
			c_count += 2;
			s_count += 1;
		} else {
			i = i + 3;
			c_count += 4;
			s_count += 1;
		}
	}

	return c_count - (s_count - 2);
}

ll diff2(std::string str) {
	ll i_count = 0, n_count = 2;  // new ""
	for (int i = 0; i < str.size(); i++) {
		char c = str[i];

		if (c == '"') {
			n_count += 2;
			i_count++;
			continue;
		}

		bool is_esc = c == '\\';
		if (is_esc) {
			n_count += 2;
			i_count++;
			continue;
		}

		n_count++;
		i_count++;
	}

	return n_count - i_count;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	ll total_diff = 0, total_diff2 = 0;
	while (getline(input_stream, line)) {
		total_diff += diff(line);
		total_diff2 += diff2(line);
	}

	std::cout << "Part 1: " << total_diff << std::endl;
	std::cout << "Part 2: " << total_diff2 << std::endl;

	return EXIT_SUCCESS;
}

