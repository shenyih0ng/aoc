#include <bitset>
#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
#include <vector>

int64_t bstr_to_int(std::string bstr) { return std::stoll(bstr, nullptr, 2); }

std::string hex_char_to_bstr(char hex_c) {
    std::stringstream hex_c_ss;
    hex_c_ss << std::hex << hex_c;
    int hex_val;
    hex_c_ss >> hex_val;
    return (new std::bitset<4>(hex_val))->to_string();
}

int get_version_sum(std::string packet, int version_sum = 0) {
    if (packet.size() < 6 || packet.find("1") == std::string::npos) {
        return version_sum;
    }

    int packet_version = bstr_to_int(packet.substr(0, 3));
    int packet_type_id = bstr_to_int(packet.substr(3, 3));
    std::string packet_data = packet.substr(6);
    if (packet_type_id == 4) {
        bool end = false;
        std::string curr_str = packet_data;
        while (!end) {
            end = curr_str[0] == '0';
            curr_str = curr_str.substr(5);
        }
        return get_version_sum(curr_str, version_sum + packet_version);
    } else {
        return packet_data[0] == '0'
                   ? get_version_sum(packet_data.substr(16),
                                     version_sum + packet_version)
                   : get_version_sum(packet_data.substr(12),
                                     version_sum + packet_version);
    }
}

int64_t eval_packet(std::string &packet) {
    int packet_version = bstr_to_int(packet.substr(0, 3));
    int packet_type_id = bstr_to_int(packet.substr(3, 3));

    packet = packet.substr(6);
    if (packet_type_id == 4) {
        bool end = false;
        std::string literal_val_str;
        while (!end) {
            literal_val_str += packet.substr(1, 4);
            end = packet[0] == '0';
            packet = packet.substr(5);
        }
        return bstr_to_int(literal_val_str);
    } else {
        int packet_len_type = packet[0];
        packet = packet.substr(1);
        std::vector<int64_t> sub_packet_values;
        if (packet_len_type == '0') {
            int sub_packet_size = bstr_to_int(packet.substr(0, 15));
            packet = packet.substr(15);
            int prev_packet_size = packet.size();
            while (sub_packet_size != 0) {
                sub_packet_values.push_back(eval_packet(packet));
                sub_packet_size -= (prev_packet_size - packet.size());
                prev_packet_size = packet.size();
            }
        } else {
            int num_sub_packets = bstr_to_int(packet.substr(0, 11));
            packet = packet.substr(11);
            while (num_sub_packets > 0) {
                sub_packet_values.push_back(eval_packet(packet));
                num_sub_packets -= 1;
            }
        }

        switch (packet_type_id) {
        case 0:
            return std::accumulate(sub_packet_values.begin(),
                                   sub_packet_values.end(), (int64_t)0);
        case 1:
            return std::accumulate(
                sub_packet_values.begin(), sub_packet_values.end(), (int64_t)1,
                [](int64_t acc, int64_t curr) { return acc * curr; });
        case 2:
            return *std::min_element(sub_packet_values.begin(),
                                     sub_packet_values.end());
        case 3:
            return *std::max_element(sub_packet_values.begin(),
                                     sub_packet_values.end());
        case 5:
            return sub_packet_values[0] > sub_packet_values[1];
        case 6:
            return sub_packet_values[0] < sub_packet_values[1];
        case 7:
            return sub_packet_values[0] == sub_packet_values[1];
        }

        return -1;
    }
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string hex_input;
    getline(input_stream, hex_input);

    std::string bin_str =
        std::accumulate(hex_input.begin(), hex_input.end(), std::string{},
                        [](std::string acc, char curr) {
                            acc += hex_char_to_bstr(curr);
                            return acc;
                        });

    std::cout << "Part 1: " << get_version_sum(bin_str) << std::endl;
    std::cout << "Part 2: " << eval_packet(bin_str) << std::endl;

    return EXIT_SUCCESS;
}
