#include <passprocvalidators.h>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

using namespace std;

typedef bool (*field_validator)(string);

string OPTIONAL_FIELDS[] = {"cid"};

map<string, field_validator> REQUIRED_FIELDS{
    {"byr", validate_byr}, {"iyr", validate_iyr}, {"eyr", validate_eyr},
    {"hgt", validate_hgt}, {"hcl", validate_hcl}, {"ecl", validate_ecl},
    {"pid", validate_pid},
};

vector<string> extract_kv_from_line(string line) {
        stringstream passport_line(line);
        string keyvalue_line;
        vector<string> kv;

        while (getline(passport_line, keyvalue_line, ' ')) {
                kv.push_back(keyvalue_line);
        }

        return kv;
}

int is_valid_field(string kv_line) {
        string key = kv_line.substr(0, 3);
        string value = kv_line.substr(0 + 3 + 1);
        if (REQUIRED_FIELDS.find(key) != REQUIRED_FIELDS.end()) {
                field_validator validate_func =
                    REQUIRED_FIELDS.find(key)->second;
                bool valid = validate_func(value);
                if (valid == 0) {
                        cout << "Key: " << key << " Value: " << value
                             << " Result: " << valid << endl;
                }

                return valid;
        }
        return false;
}

bool is_valid_passport(vector<string>& kv) {
        int num_required_fields = 0;
        for (vector<string>::iterator it = kv.begin(); it != kv.end(); ++it) {
                num_required_fields += is_valid_field(*it);
        }

        return num_required_fields == REQUIRED_FIELDS.size();
}

int main() {
        string INPUT_FILE = "./passport_processing.txt";
        ifstream input_stream(INPUT_FILE);

        int total_valid = 0;
        string line;
        if (input_stream.is_open()) {
                vector<string> passport_kv;

                while (getline(input_stream, line)) {
                        vector<string> kv = extract_kv_from_line(line);
                        passport_kv.insert(passport_kv.end(), kv.begin(),
                                           kv.end());

                        if (line.empty()) {
                                total_valid += is_valid_passport(passport_kv);
                                cout << "End of passport!" << endl;
                                passport_kv.clear();
                        }
                }

                total_valid += is_valid_passport(passport_kv);

        } else {
                cout << "Stream not open!" << endl;
        }
        cout << total_valid << endl;

        return 1;
}
