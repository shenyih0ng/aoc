#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <vector>

using namespace std;

int num_distinct_molecules(string &start,
                           map<string, vector<string> > &replacements) {
  set<string> molecules;
  for (auto r : replacements) {
    size_t r_start;
    size_t pos = 0;
    while (((r_start = start.find(r.first, pos)) != string::npos)) {
      for (auto r_str : r.second) {
        string new_molecule = start;
        new_molecule.replace(r_start, r.first.length(), r_str);
        molecules.insert(new_molecule);
      }
      pos = r_start + 1;
    }
  }

  return molecules.size();
}

pair<bool, int> num_steps(string molecule, string target, int steps,
                          vector<pair<string, string> > &replacements) {
  if (molecule == target) return make_pair(true, steps);

  for (auto r : replacements) {
    size_t r_start;
    size_t pos = 0;
    while (((r_start = molecule.find(r.first, pos)) != string::npos)) {
      string m = molecule;
      m.replace(r_start, r.first.length(), r.second);

      pair<bool, int> res = num_steps(m, target, steps + 1, replacements);
      // note: exploit greedy nature of the replacements
      if (res.first) return res;

      pos = r_start + 1;
    }
  }

  return make_pair(false, steps);
}

int main(int argc, char *argv[]) {
  ifstream input_stream(argv[1]);
  string line;

  map<string, vector<string> > replacements;
  vector<pair<string, string> > rev_replacements;

  regex re("(\\w+) => (\\w+)");
  while (getline(input_stream, line) && !line.empty()) {
    smatch match;
    regex_match(line, match, re);

    replacements[match[1]].push_back(match[2]);
    rev_replacements.push_back(make_pair(match[2], match[1]));
  }

  string molecule;
  getline(input_stream, molecule);

  cout << "Part 1: " << num_distinct_molecules(molecule, replacements) << endl;

  sort(rev_replacements.begin(), rev_replacements.end(),
       greater<pair<string, string> >());

  pair<bool, int> p2_result = num_steps(molecule, "e", 0, rev_replacements);
  cout << "Part 2: " << p2_result.second << endl;

  return EXIT_SUCCESS;
}
