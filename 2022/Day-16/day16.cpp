#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <vector>

#define START_VALVE "AA"
#define P1_TIME_LEFT 30
#define P2_TIME_LEFT 26

struct Valve {
	uint64_t flow_rate;
	std::vector<std::string> links;
};

typedef std::set<std::string> opened_t;	 // ordered for `gen_key` serialization
typedef std::map<std::string, uint64_t> cache_t;
typedef std::map<std::string, Valve> valve_map_t;

std::regex flow_regex("(\\d+)");
std::regex valve_regex("([[:upper:]]{2})");

valve_map_t valve_map;
std::map<std::string, uint64_t> cache;

std::string gen_key(std::string &v_name, opened_t &opened, int time_left,
					bool elephant) {
	std::string key = v_name;
	for (std::string ov_name : opened) key += ov_name;
	key += std::to_string(time_left);
	key += elephant;

	return key;
}

uint64_t find_max_pressure(std::string v_name, opened_t opened, int time_left,
						   bool elephant) {
	if (time_left <= 0) {
		if (elephant)
			return 0 + find_max_pressure(START_VALVE, opened, P2_TIME_LEFT,
										 false);  // thanks to reddit
		return 0;
	}

	std::string state_key = gen_key(v_name, opened, time_left, elephant);
	cache_t::iterator c_it = cache.find(state_key);
	if (c_it != cache.end()) {
		return c_it->second;
	}

	uint64_t max_p = 0;
	Valve &curr_valve = valve_map[v_name];

	// open valve if possible
	if (opened.find(v_name) == opened.end() && curr_valve.flow_rate > 0) {
		opened.insert(v_name);
		max_p = std::max(
			(time_left - 1) * curr_valve.flow_rate +
				find_max_pressure(v_name, opened, time_left - 1, elephant),
			max_p);
		opened.erase(v_name);
	}

	// don't open valve
	for (std::string l_name : curr_valve.links) {
		max_p = std::max(
			find_max_pressure(l_name, opened, time_left - 1, elephant), max_p);
	}

	cache[state_key] = max_p;
	return max_p;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	while (getline(input_stream, line)) {
		Valve valve;
		std::smatch flow_matches, valve_matches;

		std::regex_search(line, flow_matches, flow_regex);
		valve.flow_rate = std::stoi(flow_matches[0]);

		std::regex_search(line, valve_matches, valve_regex);
		std::string name = valve_matches[0];

		std::vector<std::string> links;
		std::string rest = valve_matches.suffix().str();
		while (std::regex_search(rest, valve_matches, valve_regex)) {
			links.push_back(valve_matches[0]);
			rest = valve_matches.suffix().str();
		}
		valve.links = links;
		valve_map[name] = valve;
	}

	std::cout << "Part 1: "
			  << find_max_pressure(START_VALVE, {}, P1_TIME_LEFT, false)
			  << std::endl;
	std::cout << "Part 2: "
			  << find_max_pressure(START_VALVE, {}, P2_TIME_LEFT, true)
			  << std::endl;

	return EXIT_SUCCESS;
}

