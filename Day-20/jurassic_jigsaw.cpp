#include <set>
#include <assert.h>
#include <algorithm>
#include <aocdefault.h>

#define TILE_SIZE 10

using namespace std;

vector<string> _rotate (vector<string> v) {
	vector<string> rotated(v[0].size());

	for (int c = 0; c < v[0].size(); c++) {
		string new_row;
		for (int r = 0; r < v.size(); r++) {
			new_row += v[v.size()-1-r][c];
		}
		rotated[c] = new_row;
	}

	return rotated;
}

vector<string> _flip (vector<string> v) {
	for (int idx = 0; idx < v.size(); idx++) {
		reverse(v[idx].begin(), v[idx].end());
	}

	return v;	
}

class Tile {
		int id;
		vector<string> tile;

		set<int> sides_matched;
		Tile* matches[4] = {nullptr};

		public:
			Tile (int tile_id) { id = tile_id; }

			int get_id () { return id; }

			vector<string> get_tile() { return tile; } 

			void set_tile (vector<string> arr) { tile = arr; }

			int get_num_sides_matched () { return sides_matched.size(); }

			bool match (Tile*);

			void find_match (vector<Tile*>&);

			string get_side(int side_idx) {
				if (side_idx == 0) {
					return tile[0];
				} else if (side_idx == 2) {
					return tile[tile.size()-1];
				}
				
				// this is really slow
				string side;
				for (int idx = 0; idx < tile.size(); idx++) {
					side+=tile[idx][side_idx == 1 ? tile[0].size()-1 : 0];
				}

				return side;
			}

			void set_matched_side (int side_idx, Tile* t) { 
				matches[side_idx] = t;
				sides_matched.insert(side_idx);
			}

			bool matched_with (int tile_id) {
				set<int>::const_iterator sIt = sides_matched.begin();
				for (; sIt != sides_matched.end(); sIt++) {
					if (matches[*sIt]->get_id() == tile_id) {
						return true;
					}
				}

				return false;
			}

			friend ostream& operator<< (ostream& out, Tile* t) {
					out << "id: " << t->get_id() << endl;
					vector<string> tiles = t->get_tile();
					for (int _i = 0; _i < tiles.size(); _i++) {
						cout << tiles[_i] << endl;
					}
	
					return out;
			};
};

bool tile_match (Tile* t1, Tile* t2) {
	for (int i = 0; i < 4; i++) {
		if (t1->get_side(i) == t2->get_side((i+2)%4)) {
			t1->set_matched_side(i, t2);
			t2->set_matched_side((i+2)%4, t1);

			return true;
		}
	}

	return false;
}

bool Tile::match(Tile* o_tile) {
	int count = 0;
	while (count < 4) {
		o_tile->set_tile(_rotate(o_tile->get_tile()));
		if (tile_match(this, o_tile)) {
			return true;
		}
		
		vector<string> _cache = o_tile->get_tile();	
		o_tile->set_tile(_flip(o_tile->get_tile()));
		if (tile_match(this, o_tile)) {
			return true;
		}
		
		o_tile->set_tile(_cache);

		count++;
	}

	return false;
}

void Tile::find_match (vector<Tile*>& tiles) {
	for (int idx = 0; idx < tiles.size(); idx++) {
		if (tiles[idx]->get_id() != get_id() && 
				tiles[idx]->get_num_sides_matched() != 4 &&
				!matched_with(tiles[idx]->get_id()) && 
				match(tiles[idx])) {
				tiles[idx]->find_match(tiles);
		}
	}
}

int main (int argc, char* argv[]) {
	ifstream input_stream = get_input_stream(argc, argv);
	
	vector<Tile*> tiles;

	string line;
	while(getline(input_stream, line)) {
		if (line.find("Tile") == string::npos) { continue; }

		size_t div_pos = line.find_first_of(' ');
		int tile_id = stoi(line.substr(div_pos + 1, line.size()-div_pos + 2));

		Tile* tile = new Tile(tile_id);

		size_t count = 0;
		vector<string> tile_arr;

		while(count < TILE_SIZE) {
			string tile_row;
			getline(input_stream, tile_row);

			tile_arr.push_back(tile_row);
			count++;
		};

		tile->set_tile(tile_arr);
		tiles.push_back(tile);	
	};
	
	tiles[0]->find_match(tiles); // use first tile as anchor

	// Part 1 
	long unsigned int p1_ans = 1;
	for (int i = 0; i < tiles.size(); i++) {
		p1_ans *= tiles[i]->get_num_sides_matched() == 2 ? tiles[i]->get_id() : 1;
	}
	cout << "[p1]: " << p1_ans << endl;	
}
