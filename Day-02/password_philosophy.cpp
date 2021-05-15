#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <regex>

using namespace std;

// Take note of dangling pointers
// - Understanding variables in scopes
// [ ] Understanding iterators and how pointers work in iterators
// [ ] By passing string variables to arguments - is it just the pointer or a
// copy?

regex RGX("([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-zA-Z]+)");

typedef bool (*policy_check)(int, int, char, string);

bool policy_1(int min, int max, char check_char, string password) {
        // PART 1

        int check_char_count = 0;
        for (string::iterator it = password.begin(); it != password.end();
             it++) {
                char current_char = *it;
                if (current_char == check_char) {
                        check_char_count++;
                }
        }

        return check_char_count <= max && check_char_count >= min;
}

bool policy_2(int idx1, int idx2, char check_char, string password) {
        // PART 2
        bool valid = false;

        for (int idx = 0; idx < password.length(); idx++) {
                int pos = idx + 1;
                if (pos == idx1 || pos == idx2) {
                        char letter = password[idx];
                        if (letter == check_char) {
                                valid = !valid;
                        }
                }
        }

        return valid;
}

bool check_password(string line, policy_check policy_func) {
        smatch match;

        if (regex_search(line, match, RGX)) {
                int v1 = atoi(&match[1].str()[0]);
                int v2 = atoi(&match[2].str()[0]);
                char check_char = match[3].str()[0];
                string password = match[4].str();

                return policy_func(v1, v2, check_char, password);
        } else {
                cout << "NO MATCH" << endl;
        }

        return false;
}

int main() {
        string INPUT_FILE = "./password_philosophy.txt";

        string line;
        ifstream input_stream(INPUT_FILE);
        if (input_stream.is_open()) {
                int valid = 0;
                while (getline(input_stream, line)) {
                        if (check_password(line, &policy_2)) {
                                valid++;
                        };
                }
                cout << valid << endl;
                return valid;
        }

        return 1;
}
