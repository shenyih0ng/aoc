#include <regex>
#include <vector>
#include <iostream>
#include <algorithm>
#include <unordered_map>

#include <aocdefault.h>

using namespace std;

// HELPERS

vector<int> process_ticket (string ticket) {
	vector<int> ticket_arr;
	stringstream ticket_stream(ticket);
	string ticket_val_str;

	while (getline(ticket_stream, ticket_val_str, ',')) {
		int ticket_val = atoi(ticket_val_str.c_str());
		ticket_arr.push_back(ticket_val);
	}

	return ticket_arr;
}

vector<pair<int, int>> extract_rule (string rule_line) {
	regex rules_rx("(\\d+-\\d+)");
	smatch match;
	
	vector<pair<int, int>> rules;
	regex_iterator<string::iterator> rit(rule_line.begin(), rule_line.end(), rules_rx);
	regex_iterator<string::iterator> rend;

	for (; rit != rend; ++rit) {
		string range = rit->str();
		int rspos = range.find_first_of('-');
		int n1 = atoi(range.substr(0,rspos).c_str());
		int n2 = atoi(range.substr(rspos+1).c_str());
		rules.push_back(make_pair(n1, n2));
	}

	return rules;
}

vector<pair<int, int>> reduce (vector<pair<int, int>> r1, vector<pair<int, int>> r2) {
	vector<pair<int, int>> reduced;
	reduced.insert(reduced.begin(), r1.begin(), r1.end()); // treat r1 as base

	for (int rIdx = 0; rIdx < r2.size(); rIdx++) {
		vector<int> to_pop;
		bool redunant = false;
		pair<int, int> new_rule = r2[rIdx];

		for (int bIdx = 0; bIdx < reduced.size(); bIdx++) {
			pair<int, int> rx = reduced[bIdx];
			if ((new_rule.first >= rx.first) && (new_rule.second <= rx.second)) {
				// rule is already taken care of by others
				redunant = true;
				break;
			} else if ((new_rule.first <= rx.second) && (new_rule.first >= rx.first)) {
				// min of new rule is within curr rule
				new_rule.first = rx.first;
				to_pop.push_back(bIdx);
			} else if ((new_rule.second <= rx.second) && (new_rule.second >= rx.first)) {
				new_rule.second = rx.second;
				to_pop.push_back(bIdx);
			}
		}

		for (int pIdx = 0; pIdx < to_pop.size(); pIdx++) {
			reduced.erase(reduced.begin() + to_pop[pIdx]);
		}

		if (!redunant) {
			reduced.push_back(new_rule);
		}
	}

	return reduced;
}

// Segment Tree

struct rnode {
	vector<pair<int,int>> rules;
	int lIdx;
	int rIdx;
	rnode* cl;
	rnode* cr;
	
	bool is_leaf ();
	void print (ostream&, int) const;
};


void _indent (ostream& out, int indent) {
	for (int i = 0; i < indent; i++) {
		out << "  ";
	} 
}

bool rnode::is_leaf() {
	return cl == 0 && cr == 0;
}

void rnode::print(ostream& out, int indent = 0) const {
	_indent(out, indent);
	out << lIdx << " - " << rIdx << endl;

	for (int r = 0; r < rules.size(); r++) {
		_indent(out, indent);
		pair<int, int> rule = rules[r];
		cout << rule.first << " <= x <= " << rule.second << endl;
	}
	if (cl) {
		cl->print(out, indent+1);
	}
	if (cr) {
		cr->print(out, indent+1);
	}
}

ostream& operator<< (ostream& out, const rnode& n) {
	n.print(out);
	return out;
}

rnode* build (vector<string> rules, int l, int r) {
	if (l == r) {
		rnode* nNode = new rnode();
		nNode->rIdx = r;
		nNode->lIdx = l;
		string rule = rules[l];
		nNode->rules = (extract_rule(rule));

		return nNode;
	}

	int center = (l+r)/2;
	rnode* c1 = build(rules, l, center);
	rnode* c2 = build(rules, center+1, r);

	rnode* pNode = new rnode();
	pNode->rules = reduce(c1->rules, c2->rules);
	pNode->cl = c1;
	pNode->lIdx = c1->lIdx;
	pNode->cr = c2;
	pNode->rIdx = c2->rIdx;
	
	return pNode;
}

// Core

int find_error_rate(vector<int> ticket, vector<pair<int, int>>& rules) {
	int error_rate = 0;
	for (int idx = 0; idx < ticket.size(); idx++) {
		int ticket_val = ticket[idx];
		bool valid = false;
		for (int rIdx = 0; rIdx < rules.size(); rIdx++) {
			pair<int, int> rule = rules[rIdx];
			bool _valid = (ticket_val <= rule.second) && (ticket_val >= rule.first);
			if (_valid) {
				valid = true;
				break;
			}
		}
		if (!valid) {
			error_rate += ticket_val;
		}
	}

	return error_rate;
}

int find_error_rate (vector<vector<int>> tickets, vector<pair<int, int>>& rules) {
	int t_error_rate = 0;

	for (int tIdx = 0; tIdx < tickets.size(); tIdx++) {
		t_error_rate += find_error_rate(tickets[tIdx], rules);
	}

	return t_error_rate;
}

vector<vector<int>> get_valid_tickets (vector<vector<int>>& tickets, vector<pair<int, int>>& rules) {
	vector<vector<int>> valid;
	for (int idx = 0; idx < tickets.size(); idx++) {
		if (find_error_rate(tickets[idx], rules) == 0) {
			valid.push_back(tickets[idx]);
		}
	}

	return valid;
}

vector<int> get_col (vector<vector<int>> arr, int col) {
	vector<int> col_arr;
	for (int idx = 0; idx < arr.size(); idx++) {
		col_arr.push_back(arr[idx][col]);	
	}

	return col_arr;
}

void find_matching_rules (vector<int> vals, vector<int>& rule_idx, rnode* n) {
	bool valid = find_error_rate(vals, n->rules) == 0;	
	if (valid) {
		if (n->is_leaf()) {
			rule_idx.push_back(n->lIdx);
		} else {
			if (n->cl) {
				find_matching_rules(vals, rule_idx, n->cl);
			}

			if (n->cr) {
				find_matching_rules(vals, rule_idx, n->cr);
			}
		}
	}
}

bool _compare_col_rules (vector<int> cr1, vector<int> cr2) {
	return cr1.size() < cr2.size();
}

unordered_map<int, int> get_ticket_mapping (vector<vector<int>> tickets, rnode* rTree) {
	unordered_map<int, int> results; // <rule_idx, col>

	int nCols = tickets[0].size();
	vector<vector<int>> col_valid_rules;

	for (int col = 0; col < nCols; col++) {
		vector<int> col_arr = get_col(tickets, col);
		vector<int> valid_rIdx;
		find_matching_rules(col_arr, valid_rIdx, rTree);
		valid_rIdx.insert(valid_rIdx.begin(), col); // pos 0 is reserved col value
		col_valid_rules.push_back(valid_rIdx);
	}

	sort(col_valid_rules.begin(), col_valid_rules.end(), _compare_col_rules);

	unordered_map<int, int> assigned;
	for (int idx = 0; idx < col_valid_rules.size(); ++idx) {
		vector<int> valid_rules = col_valid_rules[idx];
		for (int jdx = 1; jdx < valid_rules.size(); jdx++) {
			int rule_idx = valid_rules[jdx];
			if (assigned[rule_idx] == 0) {
				results[rule_idx] = valid_rules[0];
				assigned[rule_idx] = 1;
			}
		}
	}

	return results;
}

int main (int argc, char* argv[]) {
	ifstream input_stream(get_input_file_path(argc, argv));
	
	if (input_stream.is_open()){
		int nDiv = 1;

		vector<string> rule_names;	
		vector<string> rule_strs;

		vector<int> my_ticket;
		vector<vector<int>> nearby_tickets;

		string line;
		while (getline(input_stream, line)){
			if (line == "") {
				nDiv++;
				continue;
			}

			if (nDiv == 1) {
				int sep_pos = line.find_first_of(':');
				rule_names.push_back(line.substr(0, sep_pos));
				rule_strs.push_back(line.substr(sep_pos + 2));
			} else if (nDiv == 2) {
				if (line != "your ticket:") {
					my_ticket = process_ticket(line);
				}
			} else if (nDiv == 3) {
				if (line != "nearby tickets:") {
					nearby_tickets.push_back(process_ticket(line));
				}
			}
		}

		rnode* root = build(rule_strs, 0, rule_strs.size()-1);

		// Part 1
		int err = find_error_rate(nearby_tickets, root->rules);
		cout << "error rate: " << err << endl;

		// Part 2
		vector<vector<int>> valid_tickets = get_valid_tickets(nearby_tickets, root->rules);
		valid_tickets.push_back(my_ticket);
		
		long int departX_val = 1;
		unordered_map<int, int> ticket_mapping = get_ticket_mapping(valid_tickets, root);
		for (int idx = 0; idx < rule_names.size(); idx++) {
			string rule_name = rule_names[idx];
			int sep_pos = rule_name.find_first_of(' ');
			if (sep_pos != -1) {
				if (rule_name.substr(0, sep_pos) == "departure") {
					departX_val *= my_ticket[ticket_mapping[idx]];
				}
			}
		}
		cout << "departX: " << departX_val << endl;
	}
	
	return -1;	
}

