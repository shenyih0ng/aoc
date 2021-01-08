#include <iostream>
#include <vector>
#include <fstream>
#include <unordered_set>
#include <map>
#include <aocdefault.h>

using namespace std;

bool is_valid (long int value, vector<long int> &prev_n) {
	// Check if there is a pair of value that sums up to value in prev_n
	unordered_set<long int> lookup;
	vector<long int>::iterator it;
	for (it = prev_n.begin(); it != prev_n.end(); ++it) {
		long int num = *it;
		long int num_pair = value - num;
		if (lookup.find(num_pair) != lookup.end()) {
			return true;
		} else {
			lookup.insert(num);
		}
	}
	
	return false;
}

long int find_invalid (ifstream& stream, int preamble) {
	// Find the first number in the stream that is not valid
	vector<long int> prev_n;
	int skipped = 0;
	bool found = false;

	string line;
	long int value;
	while (!found && !stream.eof()) {
		getline(stream, line);
		value = stol(line);
		if (skipped > preamble){
			found = !is_valid(value, prev_n);		
		} else {
			skipped++;
		}
		prev_n.push_back(value);
		if (prev_n.size() > preamble) {
			prev_n.erase(prev_n.begin());
		}
	}

	if (found) { return value;}
	return -1;
}

long int find_encrypt_weakness (ifstream& stream, long int target) {
	// Find a contigous range that sums to up target -> add smallest and largest of that range
	long int encrypt_weakness;

	vector<long int> value_arr;
	map<long int, int> lookup;
	bool done = false;

	string line;
	int idx = 0;
	long int value;
	long int csum = 0;
	while (getline(stream, line) && !done) {
		value = stol(line);
		csum += value;
		value_arr.push_back(value);
		lookup.insert(make_pair(csum, idx));

		long int pair_csum = csum - target;
		auto pair_found = lookup.find(pair_csum);
		if (pair_found != lookup.end()) {
			int lower_idx = pair_found->second+1;
			if (idx >= lower_idx + 2) {
				// Calc encryption weakness
				long int min = value_arr[lower_idx];
				long int max = min;
				for (int i = lower_idx + 1; i <= idx; ++i) {
					long int val = value_arr[i];
					if (val > max) { max = val; }
					else if (val < min) { min = val; }
				}
				encrypt_weakness = max + min;
				done = true;
			}
		}
		idx++;
	}	

	return encrypt_weakness;
}

int main (int argc, char *argv[]) {	
	ifstream input_stream(get_input_file_path(argv));
	int preamble = 25;

	if (input_stream.is_open()){
		// Part 1
		long int invalid_num = find_invalid(input_stream, preamble);
		cout << "Invalid Num: " << invalid_num << endl;

		// Part 2
		input_stream.clear();
		input_stream.seekg(0, ios::beg);
		long int encryption_weakness = find_encrypt_weakness(input_stream, invalid_num);
		cout << "Encryption Weakness: " << encryption_weakness << endl;
	} else {
		cout << "Stream is not opened!" << endl;
	}

	return 1;
}
