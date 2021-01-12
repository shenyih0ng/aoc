#include <iostream>
#include <fstream>
#include <aocdefault.h>

using namespace std;

const char OCCUPIED_SEAT = '#';
const char EMPTY_SEAT = 'L';
const char FLOOR = '.';

void display (string &layout, int row_num, int lr_pad_num) {
	string::iterator it = layout.begin();
	cout << "[seats layout " << "rn: " << row_num << " pad: " << lr_pad_num << "]" << endl;
	int skipped = 0;
	int rcount = 0;
	while (it != layout.end()) {
		if (skipped < lr_pad_num) {
			skipped++;
		} else if (rcount == row_num) {
			skipped = 0;
			rcount = 0;
			cout << endl;
		} else {
			cout << *it;
			rcount++;	
		}
		it++;
	}
}

void pad_row (string &row, int lr_pad_num) {
	row.insert(0, lr_pad_num, FLOOR);
	row.insert(row.end(), lr_pad_num, FLOOR);
};

int get_num_occupied (const string &layout, int row_num, int lr_pad_num, int curr_pos) {
	// To access rows: [-rnum, 0 , +rnum]
	// 	||   cols: [-1, 0, 1]
	int occupied = 0;
	int rinc = lr_pad_num*2 + row_num;	
	for (int rd = -rinc; rd <= rinc; rd += rinc) {
		for (int cd = -1; cd <= 1; cd++) {
			if (rd != cd) {
				int qidx = curr_pos + rd + cd;
				if ((qidx >=0) && (qidx < layout.size())){
					const char qval = layout[qidx];
					occupied += (qval == OCCUPIED_SEAT);
				}
			}
		}
	}

	return occupied;
}

/*
 * Run a single cycle and return a response indicating the new state & whether a delta is observed
 */
pair<bool, string> run_cycle (const string &state, int row_num, int lr_pad_num, int &num_occupied) {
	string new_state;
	string::const_iterator it;
	bool changed = false;
	for (it = state.cbegin(); it != state.cend(); ++it) {
		const char ele = *it;
		char new_ele = ele;
		if (ele != FLOOR) {
			int num_occupied = get_num_occupied(state, row_num, lr_pad_num, it-state.cbegin());
			if ((num_occupied == 0) && (ele == EMPTY_SEAT)) {
				new_ele = OCCUPIED_SEAT;
				changed = true;
			} else if ((num_occupied >= 4) && (ele == OCCUPIED_SEAT)) {
				new_ele = EMPTY_SEAT;
				changed = true;
			}
		}
		new_state += new_ele;
		num_occupied += (new_ele == OCCUPIED_SEAT);
	}

	return make_pair(changed, new_state);
}

int main (int argc, char *argv[]) {
	ifstream input_stream(get_input_file_path(argv));
		
	if (input_stream.is_open()) {
		int row_num = -1;
		int lr_pad_num = 1;
		string seats_layout;
		string line;
		while (getline(input_stream, line)) {
			if (row_num == -1) {
				row_num = line.size();
			}
			pad_row(line, lr_pad_num);
			seats_layout.append(line);
		}

		// Part 1
		bool done = false;
		string &state = seats_layout;
		int num_occupied;
		while (!done) {	
			num_occupied = 0;
			pair<bool, string> new_state = run_cycle(state, row_num, lr_pad_num, num_occupied);
			state = new_state.second;
			done = !new_state.first;
		}
		display(state, row_num, lr_pad_num);
		cout << "num occupied(steady state): " << num_occupied << endl;
	} else {
		cout << "Stream is not opened!" << endl;
	}
	
	return 1;
}
