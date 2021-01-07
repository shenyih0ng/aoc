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
	string line;
	while ((count < n_lines) and !stream.eof()) {
		if (getline(stream, line)) {
			cache.push_back(make_pair(line, 0));
			count++;
		}
	}
	
	return stream.eof();
}

pair<string, bool> retrieve_op (int op_num, vector<pair<string, int>> &cache) {
	pair<string, int> &op_pair = cache[op_num];
	op_pair.second++;
	if (op_pair.second >= 2){
		return make_pair(op_pair.first, false);
	}

	return make_pair(op_pair.first, true);
}

void modify_last_accessed (int op_num, int modifier, vector<pair<string, int>> &cache) {
	pair<string, int> &op_pair = cache[op_num];
	op_pair.second += modifier;
}

void change_op_tag (int op_num, string new_op_tag, vector<pair<string, int>> &cache) {
	pair<string, int> &op_pair = cache[op_num];
	op_pair.first.replace(0,3,new_op_tag);
}

bool process_op (string op_str, ifstream& input_stream, vector<pair<string, int>> &cache, int &op_num, int &acc) {	
	string op_tag = op_str.substr(0,3);
	int acc_value = stoi(op_str.substr(4));

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
		return add_n_lines_to_cache(lines_to_add, input_stream, cache);
	}
	
	return false;
}

bool run (ifstream &input_stream, vector<pair<string, int>> &cache, int &op_num, int &acc) {
	bool done = false; // TODO reconsider how do we want to handle EOF (how do i know when it ends?)
	while (!done) {
		if (op_num == -1) {
			done = add_n_lines_to_cache(1, input_stream, cache);
			op_num++;
		}
		pair<string, bool> op = retrieve_op(op_num, cache);
		if (op.second == true) {
			done = process_op(op.first, input_stream, cache, op_num, acc);
		} else {
			cout << "[STOPPED] Operation accessed twice!" << endl;
			cout << "\tOp: " << op.first << " Op_Num: " << op_num << endl;
			
			return false;
		}
	}

	return done;
}

bool run_test (ifstream &input_stream, vector<pair<string, int>> &cache, int op_num, int acc) {
	bool done = false;
	vector<int> accessed_op_nums;
	while (!done) {
		if (op_num == -1) {
			done = add_n_lines_to_cache(1, input_stream, cache);
			op_num++;
		}
		pair<string, bool> op = retrieve_op(op_num, cache);
		accessed_op_nums.push_back(op_num);
		if (op.second == true) {
			done = process_op(op.first, input_stream, cache, op_num, acc);	
		} else {
			// restore access counts to all accessed_ops
			vector<int>::iterator it;
			for (it = accessed_op_nums.begin(); it != accessed_op_nums.end(); ++it){
				modify_last_accessed(*it, -1, cache);	
			}

			cout << "[STOPPED] Operation accessed twice!" << endl;
			cout << "\tOp: " << op.first << " Op_Num: " << op_num << endl;
			return false;
		}
	}

	return done;
}


void analyze_jump (ifstream &input_stream, vector<pair<string, int>> &cache) {
	int op_num = -1;
	int acc = 0;
	bool done = false;
	while (!done) {
		if (op_num == -1) {
			done = add_n_lines_to_cache(1, input_stream, cache);
			op_num++;
		}
		pair<string, bool> op = retrieve_op(op_num, cache);
		if (op.second = true) {
			string op_tag = op.first.substr(0,3);
			int acc_value = stoi(op.first.substr(4));

			int lines_to_add = 0;
			int op_num_increment = 1;
			if (op_tag == "jmp") {
				modify_last_accessed(op_num, -1, cache);
				change_op_tag(op_num, "nop", cache);

				if (!run_test(input_stream, cache, op_num, acc)) {
					modify_last_accessed(op_num, 1, cache);
					change_op_tag(op_num, "jmp", cache);

					op_num_increment = acc_value;	
				}
				
			} else if (op_tag == "acc") {
				acc += acc_value;
			}
			op_num += op_num_increment;
			if (op_num >= cache.size()) {
				lines_to_add = op_num - cache.size() + 1;
			}

			done = add_n_lines_to_cache(lines_to_add, input_stream, cache);	
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
		//run(input_stream, cache, op_num, acc);
		//cout << "Acc Value: " << acc << endl;
		//display_cache(cache);
		
		// Part 2
	} else {
		cout << "Stream is not opened!" << endl;
	}

	return 1;
}
