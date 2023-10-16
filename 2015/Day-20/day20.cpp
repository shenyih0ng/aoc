#include <fstream>
#include <iostream>

using namespace std;

typedef long long int ll;

vector<ll> get_elfs(ll house_num, int elf_house_max = -1) {
  vector<ll> elfs;

  for (ll i = 1; i <= ((ll)sqrt(house_num)); i++) {
    if (house_num % i != 0) continue;

    if (elf_house_max == -1 || house_num / i <= elf_house_max)
      elfs.push_back(i);

    ll complement = house_num / i;
    if (i != complement &&
        (elf_house_max == -1 || house_num / complement <= elf_house_max))
      elfs.push_back(complement);
  }

  return elfs;
}

ll get_num_presents(ll house_num, ll multiplier, int elf_house_max = -1) {
  vector<ll> elfs = get_elfs(house_num, elf_house_max);

  ll num_presents = 0;
  for (ll e : elfs) num_presents += e;

  return num_presents * multiplier;
}

int main(int argc, char *argv[]) {
  ifstream input_stream(argv[1]);
  string line;

  ll num_presents;
  getline(input_stream, line);
  num_presents = stoll(line);

  // note: we cannot do a binary search because number of presents does not
  // increase monotonically

  int house_num_p1 = 1, house_num_p2 = 1;
  while (get_num_presents(house_num_p1, 10) < num_presents) house_num_p1++;
  while (get_num_presents(house_num_p2, 11, 50) < num_presents) house_num_p2++;

  cout << "Part 1: " << house_num_p1 << endl;
  cout << "Part 2: " << house_num_p2 << endl;

  return EXIT_SUCCESS;
}
