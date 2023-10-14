#include <fstream>
#include <iostream>
#include <vector>

#define GRID_SIZE 100
#define NUM_STEPS 100

using namespace std;

struct Configuration {
  int size;
  bool fix_four_corners;
  vector<bool> lights;

  Configuration(int size, bool fix_four_corners) {
    this->size = size;
    this->fix_four_corners = fix_four_corners;
    lights.resize(size * size);

    if (fix_four_corners) {
      lights[0] = true;
      lights[size - 1] = true;
      lights[(size - 1) * size] = true;
      lights[(size - 1) * size + (size - 1)] = true;
    }
  }

  void set(int x, int y, bool value) {
    if (fix_four_corners) {
      if ((x == 0 && y == 0) || (x == 0 && y == size - 1) ||
          (x == size - 1 && y == 0) || (x == size - 1 && y == size - 1))
        return;
    }

    lights[y * this->size + x] = value;
  }

  bool get(int x, int y) { return lights[y * this->size + x]; }

  int num_lit() {
    int count = 0;
    for (int i = 0; i < lights.size(); i++) count += lights[i];

    return count;
  }
};

int num_lit_neighbours(Configuration& config, int x, int y) {
  int count = 0;

  for (int r = y - 1; r <= y + 1; r++) {
    for (int c = x - 1; c <= x + 1; c++) {
      if (r == y && c == x) continue;
      if (r < 0 || r >= config.size || c < 0 || c >= config.size) continue;
      count += config.get(c, r);
    }
  }

  return count;
}

void step(Configuration& config) {
  vector<pair<int, int> > to_toggle;

  for (int r = 0; r < config.size; r++) {
    for (int c = 0; c < config.size; c++) {
      bool curr_state = config.get(c, r);
      int num_nb_lit = num_lit_neighbours(config, c, r);

      if (curr_state && (num_nb_lit < 2 || num_nb_lit > 3)) {
        to_toggle.push_back(make_pair(c, r));
        continue;
      }
      if (!curr_state && num_nb_lit == 3) {
        to_toggle.push_back(make_pair(c, r));
        continue;
      }
    }
  }

  for (pair<int, int> coord : to_toggle) {
    bool curr_state = config.get(coord.first, coord.second);
    config.set(coord.first, coord.second, !curr_state);
  }
}

int main(int argc, char* argv[]) {
  ifstream input_stream(argv[1]);
  string line;

  Configuration config_p1(GRID_SIZE, false);
  Configuration config_p2(GRID_SIZE, true);

  int row = 0;
  while (getline(input_stream, line)) {
    for (int i = 0; i < line.length(); i++) {
      config_p1.set(i, row, line[i] == '#');
      config_p2.set(i, row, line[i] == '#');
    }
    row++;
  }

  int num_steps_p1 = NUM_STEPS, num_steps_p2 = NUM_STEPS;

  while (num_steps_p1--) step(config_p1);
  while (num_steps_p2--) step(config_p2);

  cout << "Part 1: " << config_p1.num_lit() << endl;
  cout << "Part 2: " << config_p2.num_lit() << endl;

  return EXIT_SUCCESS;
}
