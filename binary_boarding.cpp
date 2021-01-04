#include <iostream>
#include <algorithm>
#include <fstream>
#include <set>
#include <aocdefault.h>

using namespace std;

int get_seat_helper (string &boarding_pass, int curr_idx, int lower, int upper) {
	char c = boarding_pass[curr_idx];

	if (curr_idx == boarding_pass.size() - 1) {
		if (c == 'F' || c == 'L') {
			return lower;
		} else if (c == 'B' || c == 'R') {
			return upper;
		}	
	}

	int mid = (lower + upper) / 2;
	if (c == 'F' || c == 'L') {
		// lower half = (lower, mid)
		return get_seat_helper(boarding_pass, curr_idx+1, lower, mid);
	} else if (c == 'B' || c == 'R') {
		// upper half = (mid+1 , upper)
		return get_seat_helper(boarding_pass, curr_idx+1, mid+1, upper);
	}

	return -1;
}

int get_row (string &boarding_pass) { return get_seat_helper(boarding_pass, 0, 0, 127); }
int get_col (string &boarding_pass) { return get_seat_helper(boarding_pass, 0, 0, 7); }

int find_my_seat_id (set<int> &seat_ids) {
	for (set<int>::iterator it = seat_ids.begin(); it != seat_ids.end(); it++) {
		int curr_seat_id = *it;
		it++;
		int next_seat_id = *it;
		if ((curr_seat_id + 1) != next_seat_id) {
			return curr_seat_id + 1;
		}
	}

	return -1;
}

int main (int argc, char *argv[]) {
	ifstream input_stream(get_input_file_path(argv));

	string line;
	// *sets are always sorted
	// we could maintain a sorted vector/array with inser time log(n) but insert time will stil be o(n)
	set<int> seat_ids;
	if (input_stream.is_open()) {
		while ( getline(input_stream, line) ) {
			string row_sect = line.substr(0,7);
			string col_sect = line.substr(7);

			int row_num = get_row(row_sect);
			int col_num = get_col(col_sect);
			int seat_id = (row_num*8) + col_num;

			seat_ids.insert(seat_id);
		}
	}
	cout << find_my_seat_id(seat_ids) << endl;
	
	return 1;
}
