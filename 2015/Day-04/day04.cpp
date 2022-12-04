#include <fstream>
#include <iostream>

#include "md5.h"

long long int find_lowest_num(std::string secret, int num_zeros) {
	std::string prefix;
	for (int i = 0; i < num_zeros; i++) prefix += '0';
	long long int num = 0;
	while (true) {
		std::string hash = md5(secret + std::to_string(num));
		if (strncmp(hash.c_str(), prefix.c_str(), num_zeros) == 0) {
			return num;
		}
		num++;
	}
}

int main(int argc, char *argv[]) {
	std::string secret = "bgvyzdsv";

	std::cout << "Part 1: " << find_lowest_num(secret, 5) << std::endl;
	std::cout << "Part 1: " << find_lowest_num(secret, 6) << std::endl;

	return EXIT_SUCCESS;
}

