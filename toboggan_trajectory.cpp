#include <iostream>
#include <fstream>
#include <stdio.h>

using namespace std;

char TREE = '#';

typedef int (*move_op) (istream&, string&, int, int&);

void display (int idx, int line_length) {
	string pos_pointer = "^";
	pos_pointer.append(line_length - (idx+1), '-'); // pad rightwards
	pos_pointer.insert(pos_pointer.begin(), line_length-pos_pointer.length(), '-');

	cout << "	" << pos_pointer << endl;
}


int move_right (istream& terrain, string& prev_line, int value, int& h_idx) {
	cout << "Moving RIGHT - " << value << endl;
	
	string curr_line;
	if (prev_line.empty()) {
		getline(terrain, curr_line);
	} else {
		curr_line = prev_line;
	}

	int trees = 0;	
	if (!curr_line.empty()) {	
		cout << "	" << curr_line << endl;
		int count = 0;
		while (count < value) {
			int idx = h_idx % curr_line.length();
			
			display(idx, curr_line.length()); // display

			char position = curr_line[idx];
			trees += position == TREE;
			h_idx++;
			count++;		
		}
	}
	prev_line = curr_line;
	cout << "	Trees: " << trees << endl;	
	return trees;
}

int move_down (istream& terrain, string& prev_line, int value, int& h_idx) {
	cout << "Moving DOWN - " << value << endl;
	
	string line;
	int count = 0;
	while (count < value) {
		if (getline(terrain, line)) {
			cout << "	" << line << endl;
		} else {
			cout << "EOF" << endl;

			return 0;
		}
		count++;
	}
	int idx = h_idx % line.length();
	prev_line = line;

	display(idx, line.length()); //display
	
	return line[idx] == TREE;
}

int move (istream& terrain, int& start_h_idx, move_op* moves, int* move_values) {
	// Loop through stream goes to the bottom
	// - Only the last move operation will contribute to the count
	int trees = 0;
	string line = "";	

	while (terrain.peek() != EOF) {
		int i;
		for (i = 0; i <= (sizeof(moves)/sizeof(moves[0]))-1; i++) {
			int move_value = move_values[i];
			move_op action = moves[i];	
			
			action(terrain, line, move_value, start_h_idx) == -1;
		}

		// Operation that counts
		int last_move_value = move_values[i];
		move_op last_action = moves[i];
		trees += last_action(terrain, line, last_move_value, start_h_idx);			
	}

	cout << "Total Trees: " << trees << endl;

	return trees;
}

int main () {
	string INPUT_FILE = "./toboggan_trajectory.txt";
	ifstream terrain(INPUT_FILE);
		

	// Part 1
	//int start_h_idx = 0;
	//move_op move_operations[] = {&move_right, &move_down};	
	//int test_values[] = {3,1};
	//int total_trees = move(terrain, start_h_idx, move_operations, test_values);

	// Part 2
	move_op move_operations[] = {&move_right, &move_down};
	int test_values[][2] = {{1,1}, {3,1}, {5,1}, {7,1}, {1,2}};
	
	int num_trees_multi = 1;	
	for (int* test_value : test_values) {
		int start_h_idx = 0;
		int tree_encountered = move(terrain, start_h_idx, move_operations, test_value);
		num_trees_multi *= tree_encountered;

		// reset stream
		terrain.clear();
		terrain.seekg(0, std::ios::beg);
	}
	cout << "Num trees Multi: " << num_trees_multi << endl;

	return 1;
}
