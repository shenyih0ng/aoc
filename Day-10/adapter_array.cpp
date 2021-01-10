#include <iostream>
#include <fstream>
#include <iterator>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <set>
#include <aocdefault.h>

using namespace std;

void display (vector<int> &array) {
	cout << "[";
	copy(array.begin(), array.end(), ostream_iterator<int>(cout, " "));
	cout << "]" << endl;
}

void display (vector<vector<int>> &subarrays) {
	vector<vector<int>>::iterator it;
	for (it = subarrays.begin(); it != subarrays.end(); ++it) {
		vector<int> subarray = *it;
		display(subarray);
	}
}

int calc_jolt_diff (set<int> &ordered_set) {
	int jolt_diff_1 = 0;
	int jolt_diff_3 = 0;
	
	bool done = false;
	set<int>::iterator it = ordered_set.begin();
	while (!done) {
		int curr = *it;
		++it;
		if (it != ordered_set.end()) {
			int next = *it;
			jolt_diff_1 += (next - curr == 1);
			jolt_diff_3 += (next - curr == 3);		
		} else {
			done = true;
		}		
	}
		
	return jolt_diff_1 * jolt_diff_3;
}

vector<int> create_jolt_diff_arr (set<int> &ordered_set) {
	vector<int> jolt_diff_arr;
	set<int>::iterator it = ordered_set.begin();
	bool done = false;
	while (!done) {
		int curr = *it;
		++it;
		if (it != ordered_set.end()) {
			int next = *it;
			jolt_diff_arr.push_back(next - curr);
		} else {
			done = true;
		}
	}

	return jolt_diff_arr;
}

vector<vector<int>> get_subarrays (vector<int> &jolt_diff_arr) {
	vector<vector<int>>  all_subarrays;
	vector<int> subarray;	
	vector<int>::iterator it = jolt_diff_arr.begin();
	while (it != jolt_diff_arr.end()) {
		if (*it == 3) {
			if (!subarray.empty()){
				all_subarrays.push_back(subarray);
			}
			subarray.clear();
		} else {
			subarray.push_back(*it);
		}
		++it;
	}

	return all_subarrays;
}

long int find_num_internal_arrangement(vector<int> subarray, unordered_set<string> &lookup) {
	int curr_idx = 0;
	while (curr_idx < subarray.size()-1) {
		int curr_val = subarray[curr_idx];
		int next_val = subarray[curr_idx + 1];	
		if (curr_val + next_val <=3) {
			vector<int> sub_subarray;
			sub_subarray.insert(sub_subarray.begin(), subarray.begin(), subarray.begin() + curr_idx);
			sub_subarray.push_back(curr_val + next_val);
			sub_subarray.insert(sub_subarray.begin() + curr_idx + 1 , subarray.begin() + curr_idx + 2, subarray.end());
			
			// convert sub-subarray into string for lookup
			ostringstream sub_subarr_str;
			copy(sub_subarray.begin(), sub_subarray.end(), ostream_iterator<int>(sub_subarr_str,""));		
			string key = sub_subarr_str.str();
			if (lookup.find(key) == lookup.end()) {
				cout << "\t[sub-subarray]";
				display(sub_subarray);
				lookup.insert(key);
				find_num_internal_arrangement(sub_subarray, lookup);	
			}
		} 
		++curr_idx;
	}	
	
	return lookup.size();
}

int main (int argc, char *argv[]) {
	ifstream input_stream(get_input_file_path(argv));
	
	if (input_stream.is_open()) {
		string line;
		set<int> ordered_set;	
		ordered_set.insert(0); // charging outlet
		while (getline(input_stream, line)){
			int val = stoi(line);
			ordered_set.insert(val);
		}
		int dev = *ordered_set.rbegin() + 3; // insert device
		ordered_set.insert(dev);
		
		// Part 1	
		cout << "part 1: " << calc_jolt_diff(ordered_set) << endl << endl;
		
		// Part 2
		// The reason why part 1 asked for the jolt_diff_1 and jolt_diff_3 is a hint to solving part 2
		// 1) Construct a array of jolt_diff
		// 2) Using jolt_diff val of 3 as a "delimiter" -> get all subarrays of jolt_diff of < 3
		// 3) Calc all the possible permutations within the subarray
		// 4) Multiply num permutations per subarray
		vector<int> jolt_diff_arr = create_jolt_diff_arr(ordered_set);
		// Display
		cout << "jolt diffs" << endl;
		display(jolt_diff_arr);
		cout << endl;
		vector<vector<int>> subarrays = get_subarrays(jolt_diff_arr);

		vector<vector<int>>::iterator it;
		long int total_arrangements = 1;
		for (it = subarrays.begin(); it != subarrays.end(); ++it) {
			unordered_set<string> lookup;
			vector<int> subarray = *it;

			// Display
			cout << "[subarray] ";
			display(subarray);

			long int total_sub_arrangement = 1 + find_num_internal_arrangement(subarray, lookup);
			cout << "sub arrangements: " << total_sub_arrangement << endl << endl;
			total_arrangements *= total_sub_arrangement;
		}
		cout << "total arrangements: " << total_arrangements << endl;

	} else {
		cout << "Stream is not opened!" << endl;
	}

	return 1;
}
