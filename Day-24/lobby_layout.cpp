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

pair<float, float> operator+ (pair<float, float> lhs, pair<float, float> rhs) {
	return make_pair(lhs.first + rhs.first, lhs.second + rhs.second);
}

const regex rgx("e|se|sw|w|nw|ne");
const char* adjacent[] = {"e", "se", "sw", "w", "nw", "ne"};

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

void step (map<pair<float, float>, bool>& tiles, vector<pair<float, float>>& black_tiles) {
	map<pair<float, float>, bool> updated_tiles;
	vector<pair<float, float>> updated_black_tiles;

	map<pair<float, float>, int> white_tiles;
	
	vector<pair<float, float>>::const_iterator bIt = black_tiles.begin();
	for (; bIt != black_tiles.end(); bIt++) {
		int num_black_adj = 0;
		for (int idx = 0; idx < 6; idx++) {
			pair<float, float> adj_pos = (*bIt) + decode_step(adjacent[idx]);
			num_black_adj += tiles[adj_pos];
			if (!tiles[adj_pos]) {
				white_tiles[adj_pos] += 1;
			}
		}

		if (!(num_black_adj == 0 || num_black_adj > 2)) {
			updated_tiles[*bIt] = true;
			updated_black_tiles.push_back(*bIt);
		}
	} 

	map<pair<float, float>, int>::const_iterator wIt = white_tiles.begin();
	for (; wIt != white_tiles.end(); wIt++) {
		if (wIt->second == 2) {
			pair<float, float> w_pos = wIt->first;
			updated_black_tiles.push_back(w_pos);
			updated_tiles[w_pos] = true;
		}
	}

	// update state
	tiles = updated_tiles;
	black_tiles = updated_black_tiles;
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
			num_black += (tiles[displ_2d]) ? 1 : -1;
		}
		cout << "[p1] " << num_black << endl;	

		// Part 2
		vector<pair<float, float>> black_tiles;
		map<pair<float, float>, bool>::const_iterator mIt = tiles.begin();
		for (; mIt != tiles.end(); mIt++) {
			if (mIt->second) {
				black_tiles.push_back(mIt->first);
			}
		}

		int count = 100;
		while (count > 0) {
			step(tiles, black_tiles);
			count--;
		}
		cout << "[p2] " << black_tiles.size() << endl;
	};
}
