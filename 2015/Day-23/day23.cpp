#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

typedef vector<string> insts_t;

typedef long long int ll;

ll* reg_lhs(char reg, ll* a, ll* b) { return reg == 'a' ? a : b; }

ll load_reg(char reg, ll* a, ll* b) { return *reg_lhs(reg, a, b); }

void store_reg(char reg, ll* a, ll* b, ll val) { *(reg_lhs(reg, a, b)) = val; }

void run(insts_t& insts, ll* a, ll* b) {
  size_t pos = 0;

  while (pos < insts.size() && pos >= 0) {
    string inst = insts[pos];
    string cmd = inst.substr(0, 3);

    char reg = inst[4];
    if (cmd == "hlf") {
      store_reg(reg, a, b, load_reg(reg, a, b) / 2);
      pos++;
    } else if (cmd == "tpl") {
      store_reg(reg, a, b, load_reg(reg, a, b) * 3);
      pos++;
    } else if (cmd == "inc") {
      store_reg(reg, a, b, load_reg(reg, a, b) + 1);
      pos++;
    } else if (cmd == "jmp") {
      pos += stoi(inst.substr(4));
    } else if (cmd == "jie") {
      bool cnd = (load_reg(reg, a, b) % 2 == 0);
      pos += (cnd ? stoi(inst.substr(7)) : 1);
    } else if (cmd == "jio") {
      bool cnd = (load_reg(reg, a, b) == 1);
      pos += (cnd ? stoi(inst.substr(7)) : 1);
    }
  }
}

int main(int argc, char* argv[]) {
  ifstream input_stream(argv[1]);
  string line;

  insts_t insts;
  while (getline(input_stream, line)) insts.push_back(line);

  ll a = 0, b = 0;
  run(insts, &a, &b);

  cout << "Part 1: " << b << endl;

  a = 1;
  b = 0;
  run(insts, &a, &b);

  cout << "Part 2: " << b << endl;

  return EXIT_SUCCESS;
}
