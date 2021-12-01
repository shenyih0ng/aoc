#include <climits>
#include <iostream>
#include <regex>
#include <sstream>
#include <unordered_map>

#include <aocdefault.h>

using namespace std;

struct Rule {
        int idx;
        char to_match;
        vector<vector<int>> children;

        Rule(string);

        bool is_leaf();
};

Rule::Rule(string rule_str) {
        stringstream rule_stream(rule_str);

        bool done = false;
        string rule_set;
        while (getline(rule_stream, rule_set, '|') && !done) {
                string sub;
                vector<int> set;
                stringstream set_stream(rule_set);

                while (getline(set_stream, sub, ' ') && !done) {
                        if (sub == "") {
                                continue;
                        };

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

ostream& operator<<(ostream& out, Rule* r) {
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

bool Rule::is_leaf() { return children.empty(); }

string build_regex(Rule* qRule, unordered_map<size_t, Rule*>& rules,
                   size_t depth = 15) {
        if (depth == 0) {
                // force stop as it reaches maximum depth
                return "";
        }

        if (qRule->is_leaf()) {
                return {qRule->to_match};
        } else {
                string rgx;
                rgx += "(";
                for (int sIdx = 0; sIdx < qRule->children.size(); sIdx++) {
                        vector<int> child = qRule->children[sIdx];

                        if (sIdx) rgx += "|";
                        for (int cIdx = 0; cIdx < child.size(); cIdx++) {
                                rgx += build_regex(rules[child[cIdx]], rules,
                                                   depth - 1);
                        }
                }
                rgx += ")";
                return rgx;
        }
}

size_t get_matches(string rgx, vector<string>& msgs) {
        smatch _m;
        regex re(rgx);
        size_t matches = 0;
        for (int idx = 0; idx < msgs.size(); idx++) {
                matches += regex_match(msgs[idx], _m, re);
        }

        return matches;
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        if (input_stream.is_open()) {
                unordered_map<size_t, Rule*> rules;
                vector<string> msgs;

                string line;
                bool rule_end = false;
                while (getline(input_stream, line)) {
                        if (line == "") {
                                rule_end = true;
                        }

                        if (!rule_end) {
                                int colon_sep_pos = line.find_first_of(':');
                                int rule_idx =
                                    atoi(line.substr(0, colon_sep_pos).c_str());
                                Rule* n_rule =
                                    new Rule(line.substr(colon_sep_pos + 1));
                                n_rule->idx = rule_idx;
                                rules[rule_idx] = n_rule;
                        } else if (line != "") {
                                msgs.push_back(line);
                        }
                }
                // Part1
                string rgx = build_regex(rules[0], rules, INT_MAX);
                cout << "[p1] matches: " << get_matches(rgx, msgs) << endl;

                // Part 2
                vector<int> new_rule_8{42, 8};
                vector<int> new_rule_11{42, 11, 31};
                rules[8]->children.push_back(new_rule_8);
                rules[11]->children.push_back(new_rule_11);
                string rgx_2 = build_regex(rules[0], rules, 15);
                cout << "[p2] matches: " << get_matches(rgx_2, msgs) << endl;
        }

        return 0;
}
