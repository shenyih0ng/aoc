#include <fstream>
#include <iostream>
#include <map>
#include <regex>

typedef int64_t ll;

struct Monkey {
	bool has_val;
	ll val;
	std::string m_name_1;
	std::string m_name_2;
	std::string op;
};

typedef std::map<std::string, Monkey> m_map_t;

std::regex op_regex("(\\w+) (.) (\\w+)");

bool depend_on_humn(std::string m_name, m_map_t &m_map) {
	if (m_name == "humn") return true;

	Monkey &m = m_map[m_name];
	if (m.has_val) return false;

	return depend_on_humn(m.m_name_1, m_map) ||
		   depend_on_humn(m.m_name_2, m_map);
}

ll get_val(std::string m_name, m_map_t &m_map) {
	Monkey &curr_m = m_map[m_name];
	if (curr_m.has_val) return curr_m.val;
	ll m_val_1 = get_val(curr_m.m_name_1, m_map);
	ll m_val_2 = get_val(curr_m.m_name_2, m_map);

	switch (curr_m.op[0]) {
		case '+':
			return m_val_1 + m_val_2;
		case '-':
			return m_val_1 - m_val_2;
		case '*':
			return m_val_1 * m_val_2;
		case '/':
			return m_val_1 / m_val_2;
	}

	return -1;
}

ll solve_humn(std::string m_name, ll expected_val, m_map_t &m_map) {
	if (m_name == "humn") return expected_val;

	Monkey &curr_m = m_map[m_name];
	char op = curr_m.op[0];
	bool is_m1_const = !depend_on_humn(curr_m.m_name_1, m_map);
	bool is_m2_const = !depend_on_humn(curr_m.m_name_2, m_map);

	if (is_m1_const) {
		ll m1_val = get_val(curr_m.m_name_1, m_map);
		switch (op) {
			case '+':
				return solve_humn(curr_m.m_name_2, expected_val - m1_val,
								  m_map);
			case '-':
				return solve_humn(curr_m.m_name_2, m1_val - expected_val,
								  m_map);
			case '*':
				return solve_humn(curr_m.m_name_2, expected_val / m1_val,
								  m_map);
			case '/':
				return solve_humn(curr_m.m_name_2, m1_val / expected_val,
								  m_map);
		}

	} else {
		ll m2_val = get_val(curr_m.m_name_2, m_map);
		switch (op) {
			case '+':
				return solve_humn(curr_m.m_name_1, expected_val - m2_val,
								  m_map);
			case '-':
				return solve_humn(curr_m.m_name_1, expected_val + m2_val,
								  m_map);
			case '*':
				return solve_humn(curr_m.m_name_1, expected_val / m2_val,
								  m_map);
			case '/':
				return solve_humn(curr_m.m_name_1, expected_val * m2_val,
								  m_map);
		}
	}

	return -1;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	m_map_t m_map;
	while (getline(input_stream, line)) {
		std::string m_name = line.substr(0, line.find(':'));
		std::string op_str = line.substr(line.find(':') + 2);

		if (op_str[0] < 65) {
			m_map[m_name] = {true, std::stoi(op_str)};
		} else {
			std::smatch matches;
			std::regex_search(op_str, matches, op_regex);

			m_map[m_name] = {false, -1, matches[1], matches[3], matches[2]};
		}
	}

	std::cout << "Part 1: " << get_val("root", m_map) << std::endl;
	m_map["root"].op = "-";
	std::cout << "Part 2: " << solve_humn("root", 0, m_map) << std::endl;

	return EXIT_SUCCESS;
}

