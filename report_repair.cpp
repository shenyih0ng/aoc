#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

// Find the two entries that sum to 2020
// [X] Naive n^2 search
// 2. Binary search -> sort -> binary search

int main () {
	string INPUT_FILE = "./report_repair_input.txt";
	vector<int> entries_collection;

	// File IO
	int entry;
	ifstream entries_stream(INPUT_FILE);
	if (entries_stream.is_open()) {
		while ( entries_stream >> entry ) {
			entries_collection.push_back(entry);
		}
		entries_stream.close();
	}
	else cout << "Unable to open file";
	
	// Core Logic	
	vector<int>::iterator it;
	vector<int>::iterator inner_it;
	for (it = entries_collection.begin(); it != entries_collection.end(); it++) {
		for (inner_it = next(it); inner_it != entries_collection.end(); inner_it++) {
			if (*inner_it + *it == 2020) {
				cout << *inner_it * *it << endl;
				return *inner_it * *it;
			}
		}
	}
			

	return 0;
}
