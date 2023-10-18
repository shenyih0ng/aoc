#include <fstream>
#include <iostream>
#include <regex>

using namespace std;

typedef long long int ll;

ll arthmetic_sum(double a, double d, double n) {
  return (n / 2) * (2 * a + (n - 1.0) * d);
}

ll get_diagonal(ll row, ll col) {
  return arthmetic_sum(1, 1, col) + arthmetic_sum(col, 1, row - 1);
}

ll get_code(ll num) {
  ll code = 20151125;
  while (--num) code = (code * 252533) % 33554393;
  return code;
}

int main(int argc, char *argv[]) {
  ifstream input_stream(argv[1]);
  string line;
  getline(input_stream, line);

  regex re(
      "To continue, please consult the code grid in the manual.  Enter the "
      "code at row (\\d+), column (\\d+).");
  smatch match;
  regex_match(line, match, re);

  ll row = stoll(match[1]);
  ll col = stoll(match[2]);

  cout << "Part 1: " << get_code(get_diagonal(row, col)) << endl;

  return EXIT_SUCCESS;
}
