#include <map>
#include <regex>
#include <assert.h>

#include <aocdefault.h>

using namespace std;

ostream& operator<< (ostream& out, pair<float, float>& p) {
	out << '(';
	out << p.first << ", " << p.second;
	out << ')';

	return out;
}

pair<float, float>& operator+= (pair<float, float>& lhs, pair<float, float>& rhs) {
	lhs.first += rhs.first;
	lhs.second += rhs.second;

	return lhs;
}

const regex rgx("e|se|sw|w|nw|ne");

pair<float, float> decode_step (string step) {
	assert(step.size() <= 2);
	
	float x_displ = 0;
	float y_displ = 0;
	if (step.size() == 1) {
		x_displ += (step[0] == 'e') ? 1 : -1;
	} else {
		y_displ += (step[0] == 'n') ? 1 : -1;
		x_displ += (step[1] == 'e') ? 0.5 : -0.5;
	}

	return make_pair(x_displ, y_displ);
}

pair<float, float> find_displacement(string steps) {
	pair<float, float> displ_2d(0,0);
	string _steps = steps;
	smatch m;	

	while(regex_search(_steps, m, rgx)) {
		smatch::iterator sIt = m.begin();
		for(; sIt != m.end(); sIt++) {
			string step = *sIt;
			pair<float, float> step_displ = decode_step(step);
			displ_2d += step_displ;
		}
		_steps = m.suffix().str();
	}
	return displ_2d;
}

int main (int argc, char* argv[]) {
	ifstream input_stream(get_input_file_path(argc, argv));
	
	if (input_stream.is_open()) {
		string steps;
		int num_black = 0;
		map<pair<float, float>, bool> tiles;

		while(getline(input_stream, steps)) {
			pair<float, float> displ_2d = find_displacement(steps);
			tiles[displ_2d] = !tiles[displ_2d];
			num_black += (tiles[displ_2d] == false) ? -1 : 1;
		}
		cout << "[p1] " << num_black << endl;	
	};
}
