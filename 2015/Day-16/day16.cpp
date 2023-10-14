#include <fstream>
#include <iostream>
#include <regex>
#include <unordered_map>

using namespace std;

unordered_map<string, int> TICKER_TAPE = {
    {"children", 3}, {"cats", 7},    {"samoyeds", 2}, {"pomeranians", 3},
    {"akitas", 0},   {"vizslas", 0}, {"goldfish", 5}, {"trees", 3},
    {"cars", 2},     {"perfumes", 1}};

bool is_possible_match_p1(string items[3], int values[3]) {
  for (int i = 0; i < 3; i++) {
    unordered_map<string, int>::iterator it = TICKER_TAPE.find(items[i]);

    if (it == TICKER_TAPE.end()) continue;

    if (it->second != values[i]) return false;
  }

  return true;
}

bool is_possible_match_p2(string items[3], int values[3]) {
  for (int i = 0; i < 3; i++) {
    string key = items[i];
    unordered_map<string, int>::iterator it = TICKER_TAPE.find(key);

    if (it == TICKER_TAPE.end()) continue;

    int query = values[i], ref = it->second;

    if (key == "cats" || key == "trees") {
      if (query <= ref) return false;
    } else if (key == "pomeranians" || key == "goldfish") {
      if (query >= ref) return false;
    } else {
      if (query != ref) return false;
    }
  }

  return true;
}

int main(int argc, char *argv[]) {
  ifstream input_stream(argv[1]);
  string line;

  // note: input only has 3 items
  regex re("Sue (\\d+): (\\w+): (\\d+), (\\w+): (\\d+), (\\w+): (\\d+)");

  while (getline(input_stream, line)) {
    smatch match;
    regex_match(line, match, re);

    int sue = stoi(match[1]);
    string items[3] = {match[2], match[4], match[6]};
    int values[3] = {stoi(match[3]), stoi(match[5]), stoi(match[7])};

    // note: assume only one match
    if (is_possible_match_p1(items, values)) cout << "Part 1: " << sue << endl;
    if (is_possible_match_p2(items, values)) cout << "Part 2: " << sue << endl;
  }

  return EXIT_SUCCESS;
}
