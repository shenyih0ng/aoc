#include <aocdefault.h>
#include <bitset>
#include <cstring>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <vector>

using namespace std;

const string MASK_PREFIX = "mask";

bitset<36> prep_mask(string mask, bitset<36>& value) {
        bitset<36> bit_mask;
        for (int idx = 0; idx < mask.size(); idx++) {
                char c = mask[35 - idx];
                if (c == 'X') {
                        bit_mask.set(idx, value[idx]);
                } else {
                        bit_mask.set(idx, c == '1');
                }
        }

        return bit_mask;
}

void get_permutations(int n, bool arr[], vector<bool>& perms, int i = 0) {
        if (i == n) {
                perms.insert(perms.end(), &arr[0], &arr[n]);
        } else {
                arr[i] = 1;
                get_permutations(n, arr, perms, i + 1);

                arr[i] = 0;
                get_permutations(n, arr, perms, i + 1);
        }
}

vector<unsigned long int> get_xtra_mem_addr(string mask, int bMem) {
        vector<unsigned long int> mem_addrs;
        bitset<36> bMem_bits(bMem);

        vector<int> float_bit_pos;
        bitset<36> bitmask;
        for (int idx = 0; idx < mask.size(); idx++) {
                char c = mask[35 - idx];
                if (c == 'X') {
                        float_bit_pos.push_back(idx);
                        bitmask.set(idx, 0);
                } else {
                        bitmask.set(idx, c == '1');
                }
        }

        bitset<36> masked = bMem_bits | bitmask;
        vector<bool> permutations;
        bool p[float_bit_pos.size()];

        get_permutations(float_bit_pos.size(), p, permutations);

        for (int idx = 0; idx < permutations.size();
             idx += float_bit_pos.size()) {
                for (int jdx = 0; jdx < float_bit_pos.size(); jdx++) {
                        masked[float_bit_pos[jdx]] = permutations[jdx + idx];
                }
                mem_addrs.push_back(masked.to_ulong());
        }

        return mem_addrs;
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        if (input_stream.is_open()) {
                string line;
                string mask;

                unsigned long int sum = 0;
                map<unsigned long int, unsigned long int> memory;
                while (getline(input_stream, line)) {
                        if (!strncmp(line.c_str(), MASK_PREFIX.c_str(),
                                     MASK_PREFIX.size())) {
                                // mask
                                mask = line.substr(3 + 4);
                        } else {
                                // mem
                                unsigned long int cbrac_pos =
                                    line.find_first_of("]");
                                unsigned long int mem_pos =
                                    stoi(line.substr(4, cbrac_pos - 1));
                                unsigned long int mem_value_int =
                                    stoi(line.substr(cbrac_pos + 4));

                                // v1
                                // bitset<36> mem_value_bit(mem_value_int);
                                // bitset<36> bitmask = prep_mask(mask,
                                // mem_value_bit); unsigned long int
                                // masked_value
                                // =
                                // ((mem_value_bit^bitmask)^mem_value_bit).to_ulong();
                                // sum = sum - memory[mem_pos] + masked_value;
                                // memory[mem_pos] = masked_value;

                                // v2
                                vector<unsigned long int> mem_addrs =
                                    get_xtra_mem_addr(mask, mem_pos);
                                for (int idx = 0; idx < mem_addrs.size();
                                     idx++) {
                                        sum = sum - memory[mem_addrs[idx]] +
                                              mem_value_int;
                                        memory[mem_addrs[idx]] = mem_value_int;
                                }
                        }
                }
                cout << "sum: " << sum << endl;
        } else {
                cout << "[error] file not opened" << endl;
        }

        return 1;
}
