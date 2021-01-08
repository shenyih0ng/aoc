#include <iostream>
#include <fstream>
#include <set>
#include <vector>
#include <aocdefault.h> // TODO allow this function to just return a stream instead

// [ ] How does the os know what time is it after rebooting

using namespace std;

int find_num_qn_answered_correctly_by_all (ifstream &stream){
	// Part 2: Find the qns that all of the individuals in the groups answered correctly
	string line;
	string qns;
	bool is_new_block = true;
	int count = 0;

	bool done = false;	
	while (!done) {
		if (!getline(stream, line)) { done = true; }

		if (line.empty()){
			count += qns.size();
			qns.clear();
			is_new_block = true;
			continue;
		}

		if (is_new_block) {
			qns = line;
			is_new_block = false;
			continue;
		}
		string qns_updated;
		for (int idx=0 ; idx < line.size() ; idx++) {
			char c = line[idx];
			if (qns.find(c) != string::npos) {
				qns_updated += c;
			}
		}

		qns = qns_updated;
	}

	return count;
}


int find_num_qn_answered_correctly (ifstream &stream) {
	// Part 1: Find the number of questions answered correctly in a group	
	string line;
	int count = 0;
	set<char> qns;
	while (getline(stream,line)) {
		if (line.empty()) {
			count += qns.size();	
			qns.clear();
			continue;
		}
		for (string::iterator it = line.begin(); it != line.end() ; ++it) {
			qns.insert(*it);
		}	
	}
	count += qns.size();
	return count;
}

int main (int argc, char* argv[]) {
	ifstream input_stream (get_input_file_path(argv));

	if (input_stream.is_open()) {
		cout << find_num_qn_answered_correctly_by_all(input_stream) << endl;
	} else {
		cout << "File stream is not open!" << endl;
	}
	
	return 1;
}
