#include <stdio.h>
#include <fstream>
#include <iostream>
#include <unordered_map>

#include <aocdefault.h>

using namespace std;

const size_t END_1 = 2020;
const size_t END_2 = 30000000;

template <class T>
void read_starting_numbers(ifstream& input, T& spoken, size_t& idx,
                           size_t& last) {
        string val_str;
        size_t val;
        while (getline(input, val_str, ',')) {
                if (last != -1) {
                        spoken[last] = idx;
                        idx++;
                }
                val = atoi(val_str.c_str());
                last = val;
        }
}

/*
 * using unordered_map
 */
size_t find_val(unordered_map<size_t, size_t>& spoken, size_t& last,
                size_t& cIdx, size_t end_pos) {
        for (; cIdx < end_pos; cIdx++) {
                if (spoken.find(last) != spoken.end()) {
                        size_t nval = cIdx - spoken[last];
                        spoken[last] = cIdx;
                        last = nval;
                } else {
                        spoken[last] = cIdx;
                        last = 0;
                }
        }

        return last;
}

/*
 * using array as a lookup
 */
size_t find_val(size_t* spoken, size_t& last, size_t& cIdx, size_t end_pos) {
        for (; cIdx < end_pos; cIdx++) {
                if (spoken[last] != 0) {
                        size_t nval = cIdx - spoken[last];
                        spoken[last] = cIdx;
                        last = nval;
                } else {
                        spoken[last] = cIdx;
                        last = 0;
                }
        }

        return last;
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        size_t idx = 1;
        size_t last = -1;

        // unordered_map<size_t, size_t> spoken; // 15-16s
        /* allocate memory in heap since size of array will be over stack limit
         * (ulimit)*/
        size_t* spoken = new size_t[END_1 < END_2 ? END_2 : END_1]();  // 1-2s
        read_starting_numbers(input_stream, spoken, idx, last);

        // p1
        printf("end_pos: %d val: %d\n", END_1,
               find_val(spoken, last, idx, END_1));
        // p2
        printf("end_pos: %d val: %d\n", END_2,
               find_val(spoken, last, idx, END_2));
}
