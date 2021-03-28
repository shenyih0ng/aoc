#include <set>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <unordered_map>

#include <aocdefault.h>

using namespace std;

template<typename T>
ostream& operator<< (ostream& out, set<T>& s) {
	typename set<T>::iterator sIt = s.begin();
	out << "[";
	for (; sIt != s.end(); sIt++) {
		out << *sIt << ' ';
	}
	out << "]";

	return out;
}

struct Rule {
	char to_match;
	vector<vector<int>> children;

	Rule(string);

	bool is_leaf ();
};

Rule::Rule(string rule_str) {
	stringstream rule_stream(rule_str);
	
	bool done = false;
	string rule_set;
	while (getline(rule_stream, rule_set, '|') && !done) {
		string sub;
		vector<int> set;
		stringstream set_stream (rule_set);
		
		while(getline(set_stream, sub, ' ') && !done) {
			if (sub == "") { continue;};
			
			if (sub[0] == '"') {
				done = true;
				to_match = sub[1];
			} else {
				set.push_back(atoi(sub.c_str()));
			}
		}

		if (!set.empty()) {
			children.push_back(set);
		}
	}
}

ostream& operator<< (ostream& out, Rule* r) {
	out << "base: " << r->is_leaf() << endl;
	if (!r->is_leaf()) {
		for (int vIdx = 0; vIdx < r->children.size(); vIdx++) {
			out << r->children[vIdx] << endl;
		}
	} else {
		out << "to_match: " << r->to_match << endl;
	}

	return out;
}

bool Rule::is_leaf() {
	return children.empty();
}

set<string> combine (set<string> base, set<string> to_add) {
	set<string> combined;
	if (!base.empty()) {
		set<string>::iterator bIt = base.begin();
		for (; bIt != base.end(); ++bIt) {
			string bStr = *bIt;
			set<string>::iterator aIt = to_add.begin();
			for (; aIt != to_add.end(); ++aIt) {
				string aStr = *aIt;
				string cStr = bStr + aStr;
				combined.insert(cStr);
			}
		}
	} else {
		combined.insert(to_add.begin(), to_add.end());
	}

	return combined;
}


set<string> get_rule_matches (Rule* qRule, unordered_map<size_t, Rule*>& rules) {
	if (qRule->is_leaf()) {
		set<string> result;
		
		string _s = {qRule->to_match};	
		result.insert(_s);

		return result;
	} else {
		set<string> all_matches;
		for (int sIdx = 0; sIdx < qRule->children.size(); sIdx++) {
			vector<int> child = qRule->children[sIdx];

			set<string> section_matches;
			for (int cIdx = 0; cIdx < child.size(); cIdx++) {
				set<string> child_match = get_rule_matches(rules[child[cIdx]], rules);
				section_matches = combine(section_matches, child_match);
			}
			all_matches.insert(section_matches.begin(), section_matches.end());
		}
		return all_matches;
	}
}


int main (int argc, char* argv[]) {
	ifstream input_stream(get_input_file_path(argc, argv));

	if (input_stream.is_open()) {
		unordered_map<size_t, Rule*> rules;
		set<string> msgs;

		string line;
		bool rule_end = false;
		while(getline(input_stream, line)) {
			if (line == "") {
				rule_end = true;
			}

			if (!rule_end) {
				int colon_sep_pos = line.find_first_of(':');
				int rule_idx = atoi(line.substr(0, colon_sep_pos).c_str());
				Rule* n_rule = new Rule(line.substr(colon_sep_pos + 1));
				rules[rule_idx] = n_rule;
			} else if (line != "") {
				msgs.insert(line);
			}
		}
		set<string> matches = get_rule_matches(rules[0], rules);

		set<string> intersect;
		set_intersection(msgs.begin(), msgs.end(), matches.begin(), matches.end(), inserter(intersect, intersect.begin()));
		cout << "isize: " << intersect.size() << endl;	
	}

	return 0;
}
