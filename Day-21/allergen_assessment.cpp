#include <set>
#include <sstream>
#include <algorithm>
#include <unordered_map>

#include <aocdefault.h>

using namespace std;

set<string> to_set (string str, char delimiter) {
	stringstream ss(str);
	string ele;
	set<string> rset;
	while (getline(ss, ele, delimiter)) {
		if (ele.find_first_of(' ') == string::npos) {
			rset.insert(ele);		
		} else {
			rset.insert(ele.substr(1));	
		}
	}

	return rset;
}

/*
 * @param foods 
 * @param allergens
 * @param allergen_map		
 * @param found 		food with allergen found
 *
 */
void step (set<string> foods, set<string> allergens, 
		unordered_map<string, set<string>>& allergen_map, set<string>& found) {
	set<string>::iterator aIt = allergens.begin();
	for (;aIt != allergens.end(); aIt++) {
		unordered_map<string, set<string>>::iterator allergen_set = allergen_map.find(*aIt);
		if (allergen_set == allergen_map.end()) {
			allergen_map[*aIt] = foods;	
		} else {
			// find intersection of new and current set
			set<string> intersect;
			set_intersection(foods.begin(), foods.end(), 
					allergen_set->second.begin(), allergen_set->second.end(), 
					inserter(intersect, intersect.begin()));
			// remove food with allergen found
			set<string> diff;
			set_difference(intersect.begin(), intersect.end(),
					found.begin(), found.end(),
					inserter(diff, diff.begin()));

			if (diff.size() == 1) {
				// check if we have found the food with the allergen
				found.insert(diff.begin(), diff.end());
			}
			allergen_map[*aIt] = diff;
		}
	}

	unordered_map<string, set<string>>::iterator amIt = allergen_map.begin();
	for (;amIt != allergen_map.end(); amIt++) {
		set<string> _diff;
		set_difference(amIt->second.begin(), amIt->second.end(),
				found.begin(), found.end(),
				inserter(_diff, _diff.begin()));
		if (_diff.size() == 1) {
			found.insert(_diff.begin(), _diff.end());
		}
		amIt->second = _diff;
	}
}

int main (int argc, char* argv[]) {
	ifstream input_stream(get_input_file_path(argc, argv));

	if (input_stream.is_open()) {
		string line;

		set<string> found;
		set<string> all_foods;
		unordered_map<string, int> food_count;
		unordered_map<string, set<string>> allergen_sets;
		while(getline(input_stream, line)) {
			int apos = line.find_first_of('(');
			int epos = line.find_last_of(')');
			string foods = line.substr(0, apos-1);
			string allergens = line.substr(apos+10, epos-apos-10);

			set<string> aset = to_set(allergens, ',');
			set<string> fset = to_set(foods, ' ');
			
			all_foods.insert(fset.begin(), fset.end());
			set<string>::const_iterator fIt = fset.begin();
			for (; fIt != fset.end(); fIt++) {
				food_count[*fIt] += 1;
			}

			step(fset, aset, allergen_sets, found);
		}
		
		set<string> no_allergens;
		set_difference(all_foods.begin(), all_foods.end(),
				found.begin(), found.end(),
				inserter(no_allergens, no_allergens.begin()));

		int appearance_count = 0;
		set<string>::iterator it = no_allergens.begin();
		for (; it != no_allergens.end(); ++it) {
			appearance_count += food_count[*it];
		}
		cout << "acount: " << appearance_count << endl;
	}
}
