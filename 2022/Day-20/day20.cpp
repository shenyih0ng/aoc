#include <assert.h>

#include <fstream>
#include <iostream>
#include <vector>

#define DECRYPT_KEY 811589153

typedef int64_t ll;

struct Val {
	ll value;
	int og_idx;
};

ll mod(ll a, ll b) { return ((a % b) + b) % b; }

int index(std::vector<Val>& vals, Val& val) {
	int idx = 0;
	for (Val& v : vals) {
		if (v.value == val.value && v.og_idx == val.og_idx) return idx;
		idx++;
	}

	return -1;
}

void mix(std::vector<Val>& og_seq, std::vector<Val>& f_seq, int mix_count) {
	for (int _i = 0; _i < mix_count; _i++) {
		for (Val& og_v : og_seq) {
			if (og_v.value == 0) continue;
			int curr_idx = index(f_seq, og_v);
			f_seq.erase(f_seq.begin() + curr_idx);

			int new_idx = mod(curr_idx + og_v.value, f_seq.size());
			f_seq.insert(f_seq.begin() + new_idx, og_v);
		}
	}
}

int main(int argc, char* argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	Val zero;
	std::vector<Val> og_seq_p1, f_seq_p1;
	std::vector<Val> og_seq_p2, f_seq_p2;

	int idx = 0;
	while (getline(input_stream, line)) {
		ll value = std::stoi(line);
		Val v_p1 = {value, idx};
		Val v_p2 = {value * DECRYPT_KEY, idx};
		og_seq_p1.push_back(v_p1);
		og_seq_p2.push_back(v_p2);

		if (v_p1.value == 0) zero = v_p1;
		idx++;
	}

	f_seq_p1 = og_seq_p1;
	f_seq_p2 = og_seq_p2;

	mix(og_seq_p1, f_seq_p1, 1);
	mix(og_seq_p2, f_seq_p2, 10);

	int z_idx_p1 = index(f_seq_p1, zero);
	int z_idx_p2 = index(f_seq_p2, zero);

	ll sum_p1 = 0, sum_p2 = 0;
	std::vector<int> indexes = {1000, 2000, 3000};
	for (int i : indexes) {
		sum_p1 += f_seq_p1[mod(z_idx_p1 + i, f_seq_p1.size())].value;
		sum_p2 += f_seq_p2[mod(z_idx_p2 + i, f_seq_p2.size())].value;
	}

	std::cout << "Part 1: " << sum_p1 << std::endl;
	std::cout << "Part 2: " << sum_p2 << std::endl;

	return EXIT_SUCCESS;
}

