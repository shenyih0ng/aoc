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

void add_n_lines_to_cache (int n_lines, ifstream &stream, vector<pair<string, int>> &cache) {
	int count = 0;
	string line;
	while ((count < n_lines) and !stream.eof()) {
		if (getline(stream, line)) {
			cache.push_back(make_pair(line, 0));
			count++;	
		}
	}	
}

pair<string, bool> retrieve_op (int op_num, vector<pair<string, int>> &cache) {
	pair<string, int> &op_pair = cache[op_num];
	op_pair.second++;
	if (op_pair.second >= 2){
		return make_pair(op_pair.first, false);
	}

	return make_pair(op_pair.first, true);
}

void reset_last_accessed (vector<pair<string, int>> &cache) {
	vector<pair<string, int>>::iterator it;
	for (it = cache.begin(); it != cache.end(); ++it) {
		pair<string, int> &op_pair = *it;
		op_pair.second = 0;
	}
}

void modify_last_accessed (int op_num, int modifier, vector<pair<string, int>> &cache) {
	pair<string, int> &op_pair = cache[op_num];
	op_pair.second += modifier;
}

void change_op_tag (int op_num, string new_op_tag, vector<pair<string, int>> &cache) {
	pair<string, int> &op_pair = cache[op_num];
	op_pair.first.replace(0,3,new_op_tag);
}

void process_op (string op_str, ifstream& input_stream, vector<pair<string, int>> &cache, int &op_num, int &acc) {
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
		add_n_lines_to_cache(lines_to_add, input_stream, cache);
	}	
}

bool run (ifstream &input_stream, vector<pair<string, int>> &cache, int op_num, int &acc) {
	bool error = false;
	while (op_num < static_cast<int>(cache.size()) && !error) {
		if (op_num == -1) {
			add_n_lines_to_cache(1, input_stream, cache);
			op_num++;
		}
		pair<string, bool> op = retrieve_op(op_num, cache);
		if (op.second == true) {
			process_op(op.first, input_stream, cache, op_num, acc);
		} else {
			cout << "[STOPPED] Operation accessed twice!" << endl;
			cout << "\tOp: " << op.first << " Op_Num: " << op_num << endl;
			error = true;	
		}
	}

	return !error;
}

bool is_corrupt (ifstream &input_stream, vector<pair<string, int>> &cache, int op_num, int& acc) {
	//TODO I dont think this is the best implementation (seems awfully repetitive)
	bool error = false;
	vector<int> accessed_op_nums;
	while (op_num < static_cast<int>(cache.size()) && !error){
		if (op_num == -1) {
			add_n_lines_to_cache(1, input_stream, cache);
			op_num++;
		}
		pair<string, bool> op = retrieve_op(op_num, cache);
		accessed_op_nums.push_back(op_num);
		if (op.second == true) {
			process_op(op.first, input_stream, cache, op_num, acc);	
		} else {
			vector<int>::iterator it;
			for (it = accessed_op_nums.begin(); it != accessed_op_nums.end(); ++it){
				modify_last_accessed(*it, -1, cache);	
			}
			error = true;
		}
	}

	return error;
}

int fix_corrupt_jmp (ifstream &input_stream, vector<pair<string, int>> &cache) {
	bool found_corrupt_jmp = false;
	int op_num = -1;
	int acc = 0;
	while (op_num < static_cast<int>(cache.size()) && !found_corrupt_jmp) {
		if (op_num == -1) {
			add_n_lines_to_cache(1, input_stream, cache);
			op_num++;
		}
		pair<string, bool> op = retrieve_op(op_num, cache);
		if (op.second = true) {
			string op_tag = op.first.substr(0,3);
			int acc_value = stoi(op.first.substr(4));

			int lines_to_add = 0;
			int op_num_increment = 1;
			if (op_tag == "jmp") {
				int curr_acc = acc;
				modify_last_accessed(op_num, -1, cache);
				change_op_tag(op_num, "nop", cache);

				if (is_corrupt(input_stream, cache, op_num, acc)) {
					modify_last_accessed(op_num, 1, cache);
					change_op_tag(op_num, "jmp", cache);
					op_num_increment = acc_value;
					acc = curr_acc;
				} else {
					found_corrupt_jmp = true;
					op_num_increment = 0;
				}
				
			} else if (op_tag == "acc") {
				acc += acc_value;
			}

			op_num += op_num_increment;
			if (op_num >= cache.size()) {
				lines_to_add = op_num - cache.size() + 1;
				add_n_lines_to_cache(lines_to_add, input_stream, cache);	
			}
		}
	}

	return acc;
}

int main (int argc, char *argv[]) {
	ifstream input_stream(get_input_file_path(argv));

	vector<pair<string, int>> cache;
	if (input_stream.is_open()) {
		int acc = 0;
		int op_num = -1;
		// Part 1
		bool success = run(input_stream, cache, op_num, acc);
		cout << "Ended Run with ";
		if (!success) { cout << "error(infinite loop detected) "; }
		else { cout << "no error "; }
		cout << "| Acc Value: " << acc << endl;
		
		cout << endl;

		// Part 2
		if (!success) {
			reset_last_accessed(cache);
			int corrected_acc = fix_corrupt_jmp(input_stream, cache);	
			cout << "corrected Acc Value: " << corrected_acc << endl;
		}
	} else {
		cout << "Stream is not opened!" << endl;
	}

	return 1;
}
