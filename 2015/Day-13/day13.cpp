#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <vector>

#define ME "ME"

using namespace std;

typedef map<pair<string, string>, int> happiness_map_t;

int get_happiness_level(pair<string, string> key, happiness_map_t& hm) {
  if (key.first == ME || key.second == ME) return 0;
  return hm[key];
}

int get_total_happiness_level(vector<string>& arrangement,
                              happiness_map_t& hm) {
  int total_happiness = 0;
  size_t total_attendees = arrangement.size();

  for (size_t i = 0; i < total_attendees; i++) {
    string from = arrangement[i];
    string to = arrangement[(i + 1) % total_attendees];
    total_happiness += get_happiness_level(make_pair(from, to), hm) +
                       get_happiness_level(make_pair(to, from), hm);
  }

  return total_happiness;
}

int get_optimal_happiness(set<string>& attendees, happiness_map_t& hm) {
  int max_happiness = INT_MIN;
  vector<string> arrangement(attendees.begin(), attendees.end());

  while (next_permutation(arrangement.begin(), arrangement.end()))
    max_happiness =
        max(max_happiness, get_total_happiness_level(arrangement, hm));

  return max_happiness;
}

int main(int argc, char* argv[]) {
  ifstream input_stream("./input.txt");
  string line;

  set<string> attendees_set;
  happiness_map_t happiness_map;

  regex re(
      "(\\w+) would (\\w+) (\\d+) happiness units by sitting next to (\\w+).");

  while (getline(input_stream, line)) {
    smatch match;
    regex_match(line, match, re);

    string who = match[1];
    string with = match[4];

    attendees_set.insert(who);
    attendees_set.insert(with);

    int happiness = stoi(match[3]) * (match[2] == "gain" ? 1 : -1);
    happiness_map[make_pair(who, with)] = happiness;
  }

  cout << "Part 1: " << get_optimal_happiness(attendees_set, happiness_map)
       << endl;

  attendees_set.insert(ME);

  cout << "Part 2: " << get_optimal_happiness(attendees_set, happiness_map)
       << endl;

  return EXIT_SUCCESS;
}
