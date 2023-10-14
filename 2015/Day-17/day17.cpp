#include <fstream>
#include <iostream>
#include <vector>

#define LITRES 150

using namespace std;

int num_ways(int idx, vector<int>& containers, int liters) {
  if (liters == 0) return 1;
  if (liters < 0 || idx >= containers.size()) return 0;

  return num_ways(idx + 1, containers, liters) +
         num_ways(idx + 1, containers, liters - containers[idx]);
}

void num_min_containers_used(int idx, vector<int>& containers, int liters,
                             int num_used, int* min_used_ptr,
                             int* num_min_used_ptr) {
  if (liters == 0) {
    if (num_used < *min_used_ptr) {
      *min_used_ptr = num_used;
      *num_min_used_ptr = 1;
    } else if (num_used == *min_used_ptr) {
      *num_min_used_ptr += 1;
    }
    return;
  }

  if (liters < 0 || idx >= containers.size()) {
    return;
  }

  num_min_containers_used(idx + 1, containers, liters, num_used, min_used_ptr,
                          num_min_used_ptr);
  num_min_containers_used(idx + 1, containers, liters - containers[idx],
                          num_used + 1, min_used_ptr, num_min_used_ptr);
}

int main(int argc, char* argv[]) {
  ifstream input_stream(argv[1]);
  string line;

  vector<int> containers;
  while (getline(input_stream, line)) containers.push_back(stoi(line));

  cout << "Part 1: " << num_ways(0, containers, LITRES) << endl;

  int min_used = INT_MAX;
  int num_min_used = 0;
  num_min_containers_used(0, containers, LITRES, 0, &min_used, &num_min_used);

  cout << "Part 2: " << num_min_used << endl;

  return EXIT_SUCCESS;
}
