#include <fstream>
#include <iostream>
#include <set>

bool is_valid_pw(std::string pw) {
  bool has_incr_straight = false;
  std::set<char> pairs;

  for (int i = 0; i < pw.length(); i++) {
    if (pw[i] == 'i' || pw[i] == 'o' || pw[i] == 'l') return false;

    if (i < pw.length() - 1 && pw[i] == pw[i + 1]) pairs.insert(pw[i]);

    if (i < pw.length() - 2 && !has_incr_straight)
      has_incr_straight =
          (pw[i] == pw[i + 1] - 1) && (pw[i + 1] == pw[i + 2] - 1);
  }

  return has_incr_straight && pairs.size() >= 2;
}

std::string incr_pw(std::string pw) {
  int curr_idx = pw.length() - 1;

  while (pw[curr_idx] == 'z') {
    pw[curr_idx] = 'a';
    curr_idx--;
  }

  pw[curr_idx] += 1;
  return pw;
}

int main(int argc, char *argv[]) {
  std::ifstream input_stream("./input.txt");
  std::string curr_pw;
  getline(input_stream, curr_pw);

  while (!is_valid_pw(curr_pw)) curr_pw = incr_pw(curr_pw);

  std::cout << "Part 1: " << curr_pw << std::endl;

  curr_pw = incr_pw(curr_pw);
  while (!is_valid_pw(curr_pw)) curr_pw = incr_pw(curr_pw);

  std::cout << "Part 2: " << curr_pw << std::endl;

  return EXIT_SUCCESS;
}
