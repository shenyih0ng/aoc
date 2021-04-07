#include <algorithm>

#include <aocdefault.h>

using namespace std;

struct Cup {
	int value;
	bool picked;
	Cup* next;
	Cup(int);
};

Cup::Cup(int v) {
	value = v;
	picked = false;
	next = NULL;
}

ostream& operator<< (ostream& out, Cup* c) {
	out << "v: " << c->value;

	return out;
}

bool sort_cups (Cup* c1, Cup* c2) {
	return c1->value < c2 ->value;
}

Cup* find (vector<Cup*> cups, int val_target, int start, int end) {
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

Cup* find_destination(Cup* curr_cup) {
	vector<Cup*> unselected_cups;
	Cup* _curr_cup = curr_cup->next;
	while (_curr_cup != curr_cup) {
		if (!(_curr_cup->picked)) {
			unselected_cups.push_back(_curr_cup);
		}
		_curr_cup = _curr_cup->next;
	}

	sort(unselected_cups.begin(), unselected_cups.end(), sort_cups);
	
	Cup* destination = NULL;
	bool found = false;
	int target = curr_cup->value - 1;
	while (!found) {
		if (target < unselected_cups[0]->value) {
			destination = unselected_cups[unselected_cups.size()-1];
			found = true;
		} else {
			Cup* possible_match = find(unselected_cups, target, 0, unselected_cups.size()-1);
			if (possible_match != NULL) {
				destination = possible_match;
				found = true;
			}
			target--;
		}
	}

	return destination;	
}

void move (Cup* cup) {
	Cup* n3_cups[3];
	Cup* curr_cup = cup;
	for (int i=0; i<3; i++) {
		Cup* selected_cup = curr_cup->next;
		selected_cup->picked = true;
		n3_cups[i] = selected_cup;
		
		curr_cup = curr_cup->next;
	}

	Cup* destination = find_destination(cup);

	cup->next = n3_cups[2]->next;
	n3_cups[2]->next = destination->next;
	destination->next = n3_cups[0];

	for (int idx=0; idx<3; idx++) {
		n3_cups[idx]->picked = false;
	}
}

void play (Cup* s_cup, int num_moves) {
	int count = 0;
	Cup* c_cup = s_cup;
	while (count < num_moves) {
		move(c_cup);
		c_cup = c_cup->next;
		count++;
	}
}

int main (int argc, char* argv[]) {
	string cups_str;
	if (argc == 2) {
		cups_str = argv[1];
	} else {
		cout << "[err] invalid/empty input" << endl;
	}

	Cup* start_n = NULL;
	Cup* prev_n = NULL;
	string::iterator sIt = cups_str.begin();
	for (; sIt != cups_str.end(); sIt++) {
		Cup* node = new Cup((int)(*sIt)-48);
		if (start_n == NULL) {
			start_n = node;
		}
			
		if (prev_n != NULL) {
			prev_n->next = node;
		}
		prev_n = node;
	}
	prev_n->next = start_n; // circular

	play(start_n, 100);

	// Part 1
	Cup* cup_label_1 = NULL;
	bool found = false;
	Cup* curr_cup = start_n;
	while(!found) {
		if (curr_cup->value == 1) {
			cup_label_1 = curr_cup;
			found = true;
		}
		curr_cup = curr_cup->next;
	}

	if (cup_label_1 != NULL) {
		Cup* o_cup = cup_label_1->next;
		cout << "[p1]: ";
		while(o_cup != cup_label_1) {
			cout << o_cup->value;
			o_cup = o_cup->next;
		}
		cout << endl;
	} else {
		cout << "[err] cant find cup with label=1" << endl;
	}
}
