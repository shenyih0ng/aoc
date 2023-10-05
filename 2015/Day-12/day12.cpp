#include <fstream>
#include <iostream>
#include <regex>

long sum_numbers_in_json(std::string& json_str) {
  std::regex re("(-?\\d+)");
  std::sregex_iterator iter(json_str.begin(), json_str.end(), re);
  std::sregex_iterator end;

  long sum = 0;
  for (; iter != end; iter++) sum += std::stoi((*iter)[1]);

  return sum;
}

int main(int argc, char* argv[]) {
  std::ifstream input_stream("./input.txt");
  std::string json_str;
  getline(input_stream, json_str);

  std::ifstream pruned_input_stream("./pruned-input.txt");
  std::string pruned_json_str;
  getline(pruned_input_stream, pruned_json_str);

  std::cout << "Part 1: " << sum_numbers_in_json(json_str) << std::endl;
  std::cout << "Part 2: " << sum_numbers_in_json(pruned_json_str) << std::endl;

  return EXIT_SUCCESS;
}
