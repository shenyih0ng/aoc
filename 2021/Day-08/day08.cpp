#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <vector>

std::map<int, char> UNIQUE_NUM_MAP = {{2, '1'}, {4, '4'}, {3, '7'}, {7, '8'}};

std::string extract_output_values(std::string entry) {
    return entry.substr(entry.find('|') + 2);
}

std::string extract_signal_pattern(std::string entry) {
    return entry.substr(0, entry.find('|'));
}

bool is_unique(std::string val) {
    int len = val.size();
    return len == 2 || len == 4 || len == 3 || len == 7;
}

int find_num_unique(std::string output_str) {
    std::stringstream output_ss(output_str);
    std::string output;
    int num_unique = 0;
    while (getline(output_ss, output, ' ')) {
        num_unique += is_unique(output);
    }

    return num_unique;
}

typedef std::set<char> signal_repr_t;
typedef std::vector<signal_repr_t> signal_group_t;

template <typename T>
std::set<T> get_set_intersection(std::set<T> s1, std::set<T> s2) {
    std::set<T> intersect;
    std::set_intersection(s1.begin(), s1.end(), s2.begin(), s2.end(),
                          std::inserter(intersect, intersect.begin()));
    return intersect;
}

signal_repr_t signal_str_to_signal_repr(std::string signal_str) {
    std::set<char> signal_repr;
    std::for_each(signal_str.begin(), signal_str.end(),
                  [&signal_repr](char c) -> void { signal_repr.insert(c); });
    return signal_repr;
}

signal_group_t signal_strs_to_signal_group(std::string signal_group_str) {
    signal_group_t signal_reprs;
    std::stringstream signal_ss(signal_group_str);
    std::string signal;
    while (getline(signal_ss, signal, ' ')) {
        signal_reprs.push_back(signal_str_to_signal_repr(signal));
    }

    return signal_reprs;
}

signal_group_t get_signal_by_len(signal_group_t &signals, int len) {
    signal_group_t filtered_signals;
    std::copy_if(signals.begin(), signals.end(),
                 std::back_inserter(filtered_signals),
                 [len](signal_repr_t s) { return s.size() == len; });
    return filtered_signals;
}

signal_repr_t
get_signal_by_cmp(signal_group_t signal_group, signal_repr_t cmp_signal,
                  int num_intersect,
                  std::function<bool(signal_repr_t)> extra_pred = nullptr) {
    return *(
        std::find_if(signal_group.begin(), signal_group.end(),
                     [cmp_signal, num_intersect, extra_pred](signal_repr_t s) {
                         return get_set_intersection(s, cmp_signal).size() ==
                                    num_intersect &&
                                (extra_pred == nullptr || extra_pred(s));
                     }));
}

int decode_output(std::string entry) {
    std::string output_str = extract_output_values(entry);
    std::string signal_str = extract_signal_pattern(entry);
    signal_group_t signal_group = signal_strs_to_signal_group(signal_str);

    // cmp signals
    signal_repr_t one_repr = get_signal_by_len(signal_group, 2)[0];
    signal_repr_t four_repr = get_signal_by_len(signal_group, 4)[0];

    // distinct signal groups
    signal_group_t two_three_five = get_signal_by_len(signal_group, 5);
    signal_group_t six_nine_zero = get_signal_by_len(signal_group, 6);

    signal_repr_t three_repr = get_signal_by_cmp(two_three_five, one_repr, 2);
    signal_repr_t five_repr = get_signal_by_cmp(
        two_three_five, four_repr, 3,
        [three_repr](signal_repr_t s) { return s != three_repr; });
    signal_repr_t two_repr =
        *(std::find_if(two_three_five.begin(), two_three_five.end(),
                       [five_repr, three_repr](signal_repr_t s) {
                           return s != five_repr && s != three_repr;
                       }));
    signal_repr_t six_repr = get_signal_by_cmp(six_nine_zero, one_repr, 1);
    signal_repr_t nine_repr = get_signal_by_cmp(six_nine_zero, four_repr, 4);
    signal_repr_t zero_repr =
        *(std::find_if(six_nine_zero.begin(), six_nine_zero.end(),
                       [six_repr, nine_repr](signal_repr_t s) {
                           return s != six_repr && s != nine_repr;
                       }));

    std::map<signal_repr_t, char> non_unique_map = {
        {two_repr, '2'}, {three_repr, '3'}, {five_repr, '5'},
        {six_repr, '6'}, {nine_repr, '9'},  {zero_repr, '0'}};

    std::stringstream output_ss(output_str);
    std::string output_signal_str;
    std::string output_val_str;
    while (getline(output_ss, output_signal_str, ' ')) {
        if (is_unique(output_signal_str)) {
            output_val_str += UNIQUE_NUM_MAP[output_signal_str.size()];
        } else {
            signal_repr_t output_repr =
                signal_str_to_signal_repr(output_signal_str);
            output_val_str += non_unique_map[output_repr];
        }
    }

    return std::stoi(output_val_str);
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    int num_unique = 0;
    int output_total = 0;
    while (getline(input_stream, line)) {
        num_unique += find_num_unique(extract_output_values(line));
        output_total += decode_output(line);
    }

    std::cout << "Part 1: " << num_unique << std::endl;
    std::cout << "Part 2: " << output_total << std::endl;

    return -1;
}
