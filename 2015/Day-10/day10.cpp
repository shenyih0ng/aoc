#include <fstream>
#include <iostream>

std::string look_and_say(std::string seq) {
  std::string result = "";

  int curr_idx = 0;
  while (curr_idx < seq.size()) {
    int count = 0;
    char curr_digit = seq[curr_idx];
    while (curr_digit == seq[curr_idx]) {
      count++;
      curr_idx++;
    }
    result += std::to_string(count) + curr_digit;
  }

  return result;
}

int main(int argc, char *argv[]) {
  std::ifstream input_stream("./input.txt");
  std::string line;
  getline(input_stream, line);

  int num_iter_p1 = 40;
  int num_iter_p2 = 10;

  while (num_iter_p1--) line = look_and_say(line);
  std::cout << "Part 1: " << line.size() << std::endl;

  while (num_iter_p2--) line = look_and_say(line);
  std::cout << "Part 2: " << line.size() << std::endl;

  return EXIT_SUCCESS;
}
