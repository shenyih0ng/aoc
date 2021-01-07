#include <iostream>
#include <aocdefault.h>
#include <fstream>
#include <sstream>
#include <vector>

using namespace std;

void display_cache (vector<pair<string, int>> &cache) {
	vector<pair<string, int>>::iterator it;
	for (it = cache.begin(); it != cache.end(); ++it) {
		pair<string, int> op_pair = *it;
		cout << "Op: " << op_pair.first << " Accessed: " << op_pair.second << endl;
	}
}

bool add_n_lines_to_cache (int n_lines, ifstream &stream, vector<pair<string, int>> &cache) {
	int count = 0;
	bool eof = false;
	string line;
	while ((count < n_lines) and !eof) {
		if (getline(stream, line)) {
			cache.push_back(make_pair(line, 0));
			count++;
		} else {
			cout << "EOF." << endl;
			eof = true;
		}
	}
	
	return eof;
}

pair<string, bool> retrieve_op (int op_num, vector<pair<string, int>> &cache) {
	pair<string, int> &op_pair = cache[op_num];
	op_pair.second++;
	if (op_pair.second >= 2){
		return make_pair(op_pair.first, false);
	}

	return make_pair(op_pair.first, true);
}

void run (ifstream &input_stream, vector<pair<string, int>> &cache, int &op_num, int &acc) {
	bool done = false;
	while (!done) {
		if (op_num == -1) {
			done = add_n_lines_to_cache(1, input_stream, cache);
			op_num++;
		}
		pair<string, bool> op = retrieve_op(op_num, cache);
		if (op.second == true) {
			string op_tag = op.first.substr(0,3);
			int acc_value = stoi(op.first.substr(4));

			int lines_to_add = 0;
			int op_num_increment = 1;
			if (op_tag == "jmp") {
				op_num_increment = acc_value;	
			} else if (op_tag == "acc") {
				acc += acc_value;
			}
			op_num += op_num_increment;
			if (op_num >= cache.size()) {
				lines_to_add = op_num - cache.size() + 1;
			}

			done = add_n_lines_to_cache(lines_to_add, input_stream, cache);
		} else {
			cout << "[STOPPED] Operation accessed twice!" << endl;
			cout << "\tOp: " << op.first << " Op_Num: " << op_num << endl;
			done = true;
		}
	}
}

int main (int argc, char *argv[]) {
	ifstream input_stream(get_input_file_path(argv));

	int acc = 0;
	int op_num = -1;
	vector<pair<string, int>> cache;
	if (input_stream.is_open()) {
		// Part 1
		run(input_stream, cache, op_num, acc);
		cout << "Acc Value: " << acc << endl;
		//display_cache(cache);
		
		// Part 2
		//  - Use a single stream without reinitialization
		//  - It probably has to be a iterative search for all the JMP to see which one corrupts the flow. Thats why part 1 is to implement a stop condition when it starts the infinite loop
		
	} else {
		cout << "Stream is not opened!" << endl;
	}

	return 1;
}
