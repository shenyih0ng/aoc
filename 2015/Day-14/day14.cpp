#include <fstream>
#include <iostream>
#include <regex>

#define TIME 2503

using namespace std;

struct Reindeer {
  string name;
  int fly_speed;
  int fly_duration;
  int rest_duration;
  int points;
};

int dist_at_time(Reindeer& r, int time) {
  int num_flights = time / (r.fly_duration + r.rest_duration);
  int remaining_time = time % (r.fly_duration + r.rest_duration);

  return remaining_time < r.fly_duration
             ? r.fly_speed * (num_flights * r.fly_duration + remaining_time)
             : r.fly_speed * (num_flights + 1) * r.fly_duration;
}

int max_at_time(vector<Reindeer>& reindeers, int time) {
  int max_distance = INT_MIN;
  for (auto reindeer : reindeers)
    max_distance = max(max_distance, dist_at_time(reindeer, time));

  return max_distance;
}

int main(int argc, char* argv[]) {
  ifstream input_stream("./input.txt");
  string line;

  vector<Reindeer> reindeers;

  regex re(
      "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for "
      "(\\d+) seconds.");

  while (getline(input_stream, line)) {
    smatch match;
    regex_match(line, match, re);

    Reindeer r = {match[1], stoi(match[2]), stoi(match[3]), stoi(match[4]), 0};
    reindeers.push_back(r);
  }

  cout << "Part 1: " << max_at_time(reindeers, TIME) << endl;

  for (int t = 1; t <= TIME; t++) {
    int max_dist = max_at_time(reindeers, t);
    for (auto& reindeer : reindeers)
      if (dist_at_time(reindeer, t) == max_dist) reindeer.points++;
  }

  int max_points = INT_MIN;
  for (auto reindeer : reindeers) max_points = max(max_points, reindeer.points);

  cout << "Part 2: " << max_points << endl;

  return EXIT_SUCCESS;
}
