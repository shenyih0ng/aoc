#include <math.h>

#include <fstream>
#include <iostream>
#include <map>

typedef int64_t ll;

static std::map<char, ll> snafu_map = {
	{'2', 2}, {'1', 1}, {'0', 0}, {'-', -1}, {'=', -2}};

ll to_decimal(std::string snafu_str) {
	ll res = 0, exp = 0;
	for (int i = snafu_str.size() - 1; i >= 0; i--) {
		res += pow(5, exp) * snafu_map[snafu_str[i]];
		exp++;
	}

	return res;
}

std::string to_snafu(ll num) {
	std::string snafu = "";
	while (num > 0) {
		ll rem = num % 5;
		num = num / 5;
		if (rem >= 3) {
			snafu = (rem == 3 ? '=' : '-') + snafu;
			num++;	// carry
		} else {
			snafu = std::to_string(rem) + snafu;
		}
	}

	return snafu;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	ll sum = 0;
	while (getline(input_stream, line)) sum += to_decimal(line);

	std::cout << to_snafu(sum) << "🎉" << std::endl;

	return EXIT_SUCCESS;
}

