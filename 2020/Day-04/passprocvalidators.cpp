#include <ctype.h>
#include <string>

using namespace std;

bool validate_num(std::string value, int lower, int upper) {
        int yr = stoi(value);
        return (yr >= lower) && (yr <= upper);
}
bool validate_year(std::string value, int lower, int upper) {
        return (value.size() == 4) && validate_num(value, lower, upper);
}

bool validate_byr(std::string value) {
        return validate_year(value, 1920, 2002);
}
bool validate_iyr(std::string value) {
        return validate_year(value, 2010, 2020);
}
bool validate_eyr(std::string value) {
        return validate_year(value, 2020, 2030);
}

bool validate_hgt(std::string value) {
        std::string unit = value.substr(value.length() - 2);
        std::string hgt = value.substr(0, value.length() - 2);
        if (unit == "cm") {
                return validate_num(hgt, 150, 193);
        } else if (unit == "in") {
                return validate_num(hgt, 59, 76);
        }
        return false;
}

bool validate_hcl(std::string value) {
        if (value[0] == '#') {
                std::string hcl = value.substr(1);
                if (hcl.size() != 6) {
                        return false;
                }

                for (int idx = 0; idx < hcl.size(); idx++) {
                        char c = hcl[idx];
                        bool num_check = (c >= 48 && c <= 57);
                        bool alpha_check = (c >= 97 && c <= 102);
                        if (!(num_check || alpha_check)) {
                                return false;
                        }
                }
                return true;
        }
        return false;
}

bool validate_ecl(std::string value) {
        const std::string eye_colors[] = {"amb", "blu", "brn", "gry",
                                          "grn", "hzl", "oth"};

        for (std::string eye_color : eye_colors) {
                if (value == eye_color) {
                        return true;
                }
        }
        return false;
}

bool validate_pid(std::string value) {
        if (value.size() != 9) {
                return false;
        }
        for (int idx = 0; idx < value.size(); idx++) {
                char c = value[idx];
                if (!isdigit(c)) {
                        return false;
                }
        }
        return true;
}
