#include <filesystem>
#include <fstream>
#include <iterator>
#include <set>
#include <vector>

#include <iostream>

template <typename T>
std::ostream& operator<<(std::ostream& out, std::set<T>& s) {
        if (!s.empty()) {
                out << "[";
                copy(s.begin(), s.end(), std::ostream_iterator<T>(out, ", "));
                out << "\b\b]";
        } else {
                out << "[]";
        }

        return out;
}

template <typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& v) {
        if (!v.empty()) {
                out << "[";
                copy(v.begin(), v.end(), std::ostream_iterator<T>(out, ", "));
                out << "\b\b]";
        } else {
                out << "[]";
        }

        return out;
}

std::string get_input_file_path(int argc, char* argv[]);
std::ifstream get_input_stream(int argc, char* argv[]);
