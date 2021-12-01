#include <aocdefault.h>
#include <algorithm>

using namespace std;

#define TARGET 2020

bool find_pair(vector<int>::iterator start, vector<int>::iterator end,
               int target) {
        if ((end - start) < 0) {
                return false;
        }
        vector<int>::iterator mid = start + ((end - start) / 2);
        if (*mid == target) {
                return true;
        } else if (*mid > target) {
                return find_pair(start, mid - 1, target);
        } else if (*mid < target) {
                return find_pair(mid + 1, end, target);
        }

        return false;
}

int get_product(vector<int>& entries, int target_sum, int num_ele = 1) {
        bool _found = false;
        vector<int>::iterator vIt = entries.begin();
        while (!_found && vIt != entries.end()) {
                if (num_ele == 1) {
                        _found = find_pair(entries.begin(), entries.end() - 1,
                                           target_sum - *vIt);
                        if (_found) {
                                return (target_sum - *vIt) * (*vIt);
                        }
                } else {
                        int intermediate_p = get_product(
                            entries, target_sum - *vIt, num_ele - 1);
                        _found = intermediate_p != -1;
                        if (_found) {
                                return intermediate_p * (*vIt);
                        }
                }
                vIt++;
        }

        return -1;
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        if (input_stream.is_open()) {
                string line;
                vector<int> entries;
                while (getline(input_stream, line)) {
                        entries.push_back(atoi(line.c_str()));
                }

                sort(entries.begin(),
                     entries.end());  // sort vector for binary search

                // Part 1
                cout << "[p1] " << get_product(entries, TARGET) << endl;

                // Part 2
                cout << "[p2] " << get_product(entries, TARGET, 2) << endl;
        }

        return 0;
}
