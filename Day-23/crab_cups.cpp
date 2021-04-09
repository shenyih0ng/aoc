#include <unistd.h>
#include <algorithm>

#include <aocdefault.h>

using namespace std;

struct Cup {
	int value;
	bool picked;
	Cup* next;

	Cup(int);
	~Cup();
	void displayLL();
};

Cup::Cup(int v) {
	value = v;
	picked = false;
	next = NULL;
}

Cup::~Cup() {
	Cup* curr = curr;
	Cup* next = NULL;

	if (curr != NULL) {
		next = curr->next;
		(*curr).~Cup();
		curr = next;
	}
}

ostream& operator<< (ostream& out, Cup* c) {
	out << "v: " << c->value;

	return out;
}

void Cup::displayLL() {
	cout << this->value;
	Cup* curr = next;
	while(curr != this) {
		cout << curr->value;
		curr = curr->next;
	}
	cout << endl;
}

bool sort_cups (Cup* c1, Cup* c2) {
	return c1->value < c2 ->value;
}

// Binary search
Cup* find (vector<Cup*>& cups, int val_target, int start, int end) {
	if (end < start) {
		return NULL;
	}
	int mid = (start + end)/2;
	Cup* m_cup = cups[mid];
	if (m_cup->value == val_target) {
		return m_cup;
	} else if (m_cup->value > val_target) {
		return find(cups, val_target, start, mid-1);
	} else {
		return find(cups, val_target, mid+1, end);
	}
}

// find minimum/maximum value of amongst unselected cups
Cup* get_unselected_minmax (vector<Cup*>& cups, bool min=true) {
	vector<Cup*>::iterator cIt = (min) ? cups.begin() : cups.end()-1;
	Cup* target_cup = NULL;
	bool found = false;
	while(cIt != cups.end() && !found) {
		if (!(*cIt)->picked) {
			target_cup = *cIt;
			found = true;
		}
		if (min) {
			cIt++;
		} else {
			cIt--;
		}	
	}
	
	return target_cup;
}

Cup* find_destination(Cup* curr_cup, vector<Cup*>& cups) {	
	Cup* destination = NULL;
	bool found = false;
	int target = curr_cup->value - 1;
	Cup* min_unselected = get_unselected_minmax(cups);
	Cup* max_unselected = get_unselected_minmax(cups, false);
	while (!found) {
		if (target < min_unselected->value) {
			destination = max_unselected;
			found = true;
		} else {
			Cup* possible_match = find(cups, target, 0, cups.size()-1);
			if (possible_match != NULL && !possible_match->picked) {
				destination = possible_match;
				found = true;
			}
			target--;
		}
	}

	return destination;	
}

void move (Cup* cup, vector<Cup*>& cups) {
	Cup* n3_cups[3];
	Cup* curr_cup = cup;
	for (int i=0; i<3; i++) {
		Cup* selected_cup = curr_cup->next;
		selected_cup->picked = true;
		n3_cups[i] = selected_cup;
		
		curr_cup = curr_cup->next;
	}

	Cup* destination = find_destination(cup, cups);

	cup->next = n3_cups[2]->next;
	n3_cups[2]->next = destination->next;
	destination->next = n3_cups[0];

	for (int idx=0; idx<3; idx++) {
		n3_cups[idx]->picked = false;
	}
}

void play (Cup* s_cup, vector<Cup*>& cups, int num_moves) {
	int count = 0;
	Cup* c_cup = s_cup;
	while (count < num_moves) {
		move(c_cup, cups);
		c_cup = c_cup->next;
		count++;
	}
}

void form_LL (string cups_str, Cup*& start_cup, Cup*& end_cup, Cup*& cup_L1, vector<Cup*>& cups) {
	Cup* prev_n = NULL;
	string::iterator sIt = cups_str.begin();
	for (; sIt != cups_str.end(); sIt++) {
		int cup_L = (int)(*sIt)-48;
		Cup* node = new Cup(cup_L);
		if (start_cup == NULL) {
			start_cup = node;
		}
			
		if (prev_n != NULL) {
			prev_n->next = node;
		}

		if (cup_L == 1) {
			cup_L1 = node;
		}
		cups.push_back(node);
		prev_n = node;
	}
	prev_n->next = start_cup;	
	sort(cups.begin(), cups.end(), sort_cups);
	end_cup = prev_n;
}

int main (int argc, char* argv[]) {
	string cups_str;
	if (argc == 2) {
		cups_str = argv[1];
	} else {
		cout << "[err] invalid/empty input" << endl;
	}

	// Part 1
	Cup* cup_L1 = NULL;
	Cup* start_cup = NULL;
	Cup* end_cup = NULL;
	vector<Cup*> cups;

	form_LL(cups_str, start_cup, end_cup, cup_L1, cups);
	play(start_cup, cups, 100);
	if (cup_L1 != NULL) {
		Cup* o_cup = cup_L1->next;
		cout << "[p1]: ";
		while(o_cup != cup_L1) {
			cout << o_cup->value;
			o_cup = o_cup->next;
		}
		cout << endl;
	} else {
		cout << "[err] cant find cup with label=1" << endl;
	}
	
	//TODO release memory (this does not work!)
	delete start_cup;
	cups.clear();	

	// Part 2
	cup_L1 = NULL;
	start_cup = NULL;
	end_cup = NULL;

	form_LL(cups_str, start_cup, end_cup, cup_L1, cups);
	end_cup->next = NULL;
	Cup* prev_n = end_cup;
	int curr_max_val = cups[cups.size()-1]->value;
	while(curr_max_val < 1000000) {
		curr_max_val++;
		Cup* new_cup = new Cup(curr_max_val);
		prev_n->next = new_cup;
		prev_n = new_cup;

		cups.push_back(new_cup);
	}
	prev_n->next = start_cup;

	play(start_cup, cups, 10000000);
	
	long long int v1 = cup_L1->next->value;
	long long int v2 = cup_L1->next->next->value;	
	cout << "[p2]: " << v1*v2 << endl;	
}
