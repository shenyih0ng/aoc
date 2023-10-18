#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

typedef long long int ll;
typedef pair<int, ll> group_t; // (num_packages, entanglement)

group_t min_grp(group_t g1, group_t g2) {
  if (g1.first < g2.first) return g1;
  if (g1.first == g2.first) return (g1.second <= g2.second) ? g1 : g2;
  return g2;
}

group_t find_first_group(int idx, vector<ll>& packages, ll target_weight,
                         ll entanglement, int num_packages) {
  if (target_weight == 0) return make_pair(num_packages, entanglement);
  if (target_weight < 0 || idx >= packages.size())
    return make_pair(INT_MAX, INT_MAX);

  group_t min_g1 =
      find_first_group(idx + 1, packages, target_weight - packages[idx],
                       entanglement * packages[idx], num_packages + 1);
  group_t min_g2 = find_first_group(idx + 1, packages, target_weight,
                                    entanglement, num_packages);

  return min_grp(min_g1, min_g2);
}

int main(int argc, char* argv[]) {
  ifstream input_stream(argv[1]);
  string line;

  ll total = 0;
  vector<ll> packages;
  while (getline(input_stream, line)) {
    ll package = stoll(line);
    packages.push_back(package);
    total += package;
  }

  group_t first_grp_p1 = find_first_group(0, packages, total / 3, 1, 0);
  group_t first_grp_p2 = find_first_group(0, packages, total / 4, 1, 0);

  cout << "Part 1: " << first_grp_p1.second << endl;
  cout << "Part 2: " << first_grp_p2.second << endl;

  return EXIT_SUCCESS;
}
