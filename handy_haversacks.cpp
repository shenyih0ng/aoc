#include <iostream>
#include <fstream>
#include <regex>
#include <unordered_map>
#include <set>
#include <stdlib.h>
#include <aocdefault.h>

using namespace std;

string TARGET_BAG = "shiny gold";
regex ROOT_RGX ("([a-zA-Z ]+) bags contain (.+)");
regex CHILD_RGX ("((\\d+) ([a-zA-Z ]+)) bags?");

/*
 * 1. Parsing lines <colors> bags contains [<num> <color> bags] -> delimited by ,
 * 2. Generating pairwise map of every relationship
 * TODO what does '->' mean
 *
 * map<string, map<string, int>>
 */


void display_index (unordered_map<string, vector<string>> &index) {
	unordered_map<string, vector<string>>::iterator it;
	for (it = index.begin(); it != index.end(); ++it) {
		cout << "Child: " << it -> first << endl;
		vector<string> parents = it -> second;
		for (vector<string>::iterator v_it = parents.begin(); v_it!=parents.end(); ++v_it) {
			cout << "\t" << *v_it << endl;
		}
		cout << endl;
	}
}

void display_index (unordered_map<string, unordered_map<string, int>> &index) {
	unordered_map<string, unordered_map<string, int>>::iterator it;
	for (it = index.begin(); it != index.end(); ++it) {
		cout << "Parent: " << it -> first << endl;
		unordered_map<string, int> children_map = it -> second;

		unordered_map<string, int>::iterator children_itr;
		for (children_itr = children_map.begin(); children_itr != children_map.end(); ++children_itr) {
			cout << "\t" << children_itr -> first << " " << children_itr -> second << endl;
		}
		cout << endl;
	}
}

void build_index (string &line, unordered_map<string, vector<string>> &index) {
	// Part 1: index of child -> parent
	smatch root_match;
	if (regex_search(line, root_match, ROOT_RGX)) {
		string root = root_match[1];
		string children = root_match[2];

		smatch child_match;
		while (regex_search(children, child_match, CHILD_RGX)) {
			string child = child_match[3];
			unordered_map<string, vector<string>>::const_iterator got = index.find(child);
			if (got != index.end()) {
				index[child].push_back(root);
			} else {
				vector<string> parents;
				parents.push_back(root);
				index[child] = parents;
			}
			children = child_match.suffix();	
		}	
	}	
}

void build_index (string &line, unordered_map<string, unordered_map<string, int>> &index) {
	// Part 2: index of parent -> child
	// Need to consider 'leaf' bags 'contain no other bags' -> no child_match means no other bags
	smatch root_match;
	if (regex_search(line, root_match, ROOT_RGX)) {
		string root = root_match[1];
		string children = root_match[2];
		
		bool has_match = false;
		smatch child_match;
		while (regex_search(children, child_match, CHILD_RGX)) {
			has_match = true; // hacky
			int child_value = stoi(child_match[2]);
			string child = child_match[3];
			unordered_map<string, unordered_map<string, int>>::const_iterator got = index.find(root);
			if (got != index.end()) {
				unordered_map<string, int> children_map = got -> second; // TODO why does this not act like a reference
				children_map[child] = child_value;
				index[root] = children_map;
			} else {
				unordered_map<string, int> children_map;
				children_map[child] = child_value;
				index[root] = children_map;
			}
			children = child_match.suffix();
		}

		if (!has_match) {
			unordered_map<string, int> dummy_map;
			index[root] = dummy_map;
		}
	}	
}


set<string> find_bags_contain (unordered_map<string, vector<string>> &index, string target) {
	// Part 1
	unordered_map<string, vector<string>>::const_iterator target_bag = index.find(target);
	set<string> bags;
	if (target_bag != index.end()) {
		vector<string> parents = target_bag -> second;
		for (vector<string>::iterator it = parents.begin(); it!=parents.end(); ++it) {
			bags.insert(*it);
			set<string> parent_bags = find_bags_contain(index, *it);
			bags.insert(parent_bags.begin(), parent_bags.end());
		}		
	}
	
	return bags;
}


int find_num_bags_in_bag (unordered_map<string, unordered_map<string,int>> &index, string target) {
	unordered_map<string, unordered_map<string,int>>::const_iterator target_bag = index.find(target);
	int count = 1; // to count the current target, however we will need to -1 to not consider the root target. hmmm
	if (target_bag != index.end()) {
		unordered_map<string, int> children_map = target_bag -> second;
		if (children_map.size() > 0){
			unordered_map<string, int>::iterator it;
			for (it = children_map.begin(); it != children_map.end(); ++it) {
				string child = it -> first;
				int child_value = it -> second;
				count += (child_value * find_num_bags_in_bag(index, child));
			}
		}
	} else {
		cout << target << " not found!" << endl;
	}

	return count;	
}

int main (int argc, char *argv[]) {
	ifstream input_stream(get_input_file_path(argv));

	unordered_map<string, vector<string>> index_p1;	
	unordered_map<string, unordered_map<string, int>> index_p2;
	string line;	
	if (input_stream.is_open()) {
		while (getline(input_stream, line)) {
			build_index(line, index_p1);
			build_index(line, index_p2);
		}
	} else {
		cout << "Cannot open stream! Check file input" << endl;
	}
	
	//display_index(index_p2);

	cout << "Num bags: " << find_bags_contain(index_p1, TARGET_BAG).size() << endl;	
	cout << "Num bags: " << find_num_bags_in_bag(index_p2, TARGET_BAG) - 1<< endl;

	return 1;
}
