#include <fstream>
#include <iostream>
#include <regex>
#include <vector>

#define PART_2	// PART_2

#ifdef PART_2
#define ROUNDS 10000
#else
#define ROUNDS 20
#endif

std::regex digits_regex("(\\d+)");
std::regex op_regex("old (.) (.+)");

typedef std::pair<std::string, long int> op_t;

struct Monkey {
	std::vector<long int> items;
	op_t operation;
	int test_val;	  // assumed to be div test_val, if -1 -> old val
	int test_pos_mk;  // throw to when true
	int test_neg_mk;  // throw to when false
	long int num_inspected = 0;
};

std::vector<long int> extract_items(std::string &items_str) {
	std::vector<long int> items;
	std::smatch match;
	while (std::regex_search(items_str, match, digits_regex)) {
		items.insert(items.begin(), std::stoi(match[0]));
		items_str = match.suffix();
	}
	return items;
}

op_t extract_op(std::string &op_str) {
	std::smatch matches;
	std::regex_search(op_str, matches, op_regex);
	op_t res = std::make_pair(matches[1],
							  matches[2] == "old" ? -1 : std::stoi(matches[2]));
	return res;
}

int extract_test_val(std::string &str) {
	std::smatch matches;
	std::regex_search(str, matches, digits_regex);
	return std::stoi(matches[1]);
}

void play_round(std::vector<Monkey> &monkeys, int mod_prod) {
	for (int m_idx = 0; m_idx < monkeys.size(); m_idx++) {
		Monkey &curr_monkey = monkeys[m_idx];
		int num_curr_items = curr_monkey.items.size();
		for (int _item = 0; _item < num_curr_items; _item++) {
			long int curr_item = curr_monkey.items.back();
			curr_monkey.items.pop_back();

			long int op_val = curr_monkey.operation.second == -1
								  ? curr_item
								  : curr_monkey.operation.second;
			long int new_item = curr_monkey.operation.first[0] == '+'
									? curr_item + op_val
									: curr_item * op_val;
#ifndef PART_2
			new_item = new_item / 3;
#else
			// (a mod kn) mod n === a mod n
			new_item = new_item % mod_prod;
#endif
			int next_mk_idx = new_item % curr_monkey.test_val == 0
								  ? curr_monkey.test_pos_mk
								  : curr_monkey.test_neg_mk;
			Monkey &next_mk = monkeys[next_mk_idx];
			next_mk.items.insert(next_mk.items.begin(), new_item);

			curr_monkey.num_inspected++;
		}
	}
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;

	std::vector<Monkey> monkeys;
	while (getline(input_stream, line)) {
		if (line.substr(0, 6) == "Monkey") {
			std::string items_str, op_str, test_str, test_pos_str, test_neg_str;
			getline(input_stream, items_str);
			getline(input_stream, op_str);
			getline(input_stream, test_str);
			getline(input_stream, test_pos_str);
			getline(input_stream, test_neg_str);
			monkeys.push_back({extract_items(items_str), extract_op(op_str),
							   extract_test_val(test_str),
							   extract_test_val(test_pos_str),
							   extract_test_val(test_neg_str)});
		}
	}

	int mod_prod = 1;
	for (Monkey m : monkeys) mod_prod *= m.test_val;

	std::vector<long int> mk_inspects;
	for (int _i = 0; _i < ROUNDS; _i++) play_round(monkeys, mod_prod);

	for (Monkey m : monkeys) mk_inspects.push_back(m.num_inspected);
	std::sort(mk_inspects.begin(), mk_inspects.end());

	std::cout << "mk business: "
			  << mk_inspects[mk_inspects.size() - 1] *
					 mk_inspects[mk_inspects.size() - 2]
			  << std::endl;

	return EXIT_SUCCESS;
}

