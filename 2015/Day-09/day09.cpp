#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <string>

typedef std::map<std::pair<std::string, std::string>, int> distances_t;

int get_distance(std::string from, std::string to, distances_t& distances) {
  distances_t::iterator it = distances.find(std::make_pair(from, to));
  if (it != distances.end()) return it->second;

  it = distances.find(std::make_pair(to, from));
  if (it != distances.end()) return it->second;

  return -1;
}

std::pair<int, int> get_extreme_routes(std::set<std::string>& cities,
                                       distances_t& distances) {
  std::vector<std::string> cities_vec(cities.begin(), cities.end());

  int shortest_route = INT_MAX;
  int longest_route = INT_MIN;

  while (std::next_permutation(cities_vec.begin(), cities_vec.end())) {
    int route_dist = 0;

    for (int i = 0; i < cities_vec.size() - 1; ++i)
      route_dist += get_distance(cities_vec[i], cities_vec[i + 1], distances);

    shortest_route = std::min(shortest_route, route_dist);
    longest_route = std::max(longest_route, route_dist);
  }

  return std::make_pair(shortest_route, longest_route);
}

int main(int argc, char* argv[]) {
  std::ifstream input_stream("./input.txt");
  std::string line;

  std::set<std::string> cities;
  distances_t distances;

  while (getline(input_stream, line)) {
    std::smatch sm;
    std::regex_match(line, sm, std::regex("(.+) to (.+) = (.+)"));
    distances[std::make_pair(sm[1], sm[2])] = std::stoi(sm[3]);

    cities.insert(sm[1]);
    cities.insert(sm[2]);
  }

  std::pair<int, int> extreme_routes = get_extreme_routes(cities, distances);

  std::cout << "Part 1: " << extreme_routes.first << std::endl;
  std::cout << "Part 2: " << extreme_routes.second << std::endl;

  return EXIT_SUCCESS;
}
