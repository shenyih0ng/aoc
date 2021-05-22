#include <bitset>
#include <assert.h>
#include <algorithm>
#include <aocdefault.h>

#define TILE_SIZE 10

using namespace std;

bitset<TILE_SIZE> reverse (bitset<TILE_SIZE> b) {
	string str = b.to_string();
	reverse(str.begin(), str.end());
	bitset<TILE_SIZE> reversed = bitset<TILE_SIZE>(str);

	return reversed;
}

bool match_edge (bitset<TILE_SIZE> bs1, bitset<TILE_SIZE> bs2) {
	return bs1 == bs2 ||
		   reverse(bs1) == bs2 ||
		   reverse(bs2) == bs1;
}

class Tile {
		public:
			int tile_id;
			bitset<TILE_SIZE>* camera_arr;
			bitset<TILE_SIZE>* camera_edges;

			set<int> unmatched_edges = {0,1,2,3};
			Tile* matches[4] = {nullptr};

			Tile (int id, bitset<TILE_SIZE>* c_arr, bitset<TILE_SIZE>* edges) {
				tile_id = id;
				camera_arr = c_arr;
				camera_edges = edges;
			};

			size_t get_num_matched () {
				return 4-unmatched_edges.size();
			}

			friend ostream& operator<< (ostream& out, Tile* t) {
					out << "id: " << t->tile_id << endl;

					bitset<TILE_SIZE>* c_arr = t->camera_arr;
					for (int _i = 0; _i < TILE_SIZE; _i++) {
						out << c_arr[_i] << endl;
					}
	
					return out;
			};

			bool match (Tile*);
};

bool Tile::match(Tile* t) {
	set<int> local_unmatched_edges = this->unmatched_edges;
	set<int>::const_iterator local_it = local_unmatched_edges.begin();

	for (; local_it != local_unmatched_edges.end(); local_it++) {
		set<int> ext_unmatched_edges = t->unmatched_edges;
		set<int>::const_iterator ext_it = ext_unmatched_edges.begin();
		for (; ext_it != ext_unmatched_edges.end(); ext_it++) {
			if (match_edge(this->camera_edges[*local_it], t->camera_edges[*ext_it])) {
				this->matches[*local_it] = t;
				t->matches[*ext_it] = this;
				this->unmatched_edges.erase(*local_it);
				t->unmatched_edges.erase(*ext_it);

				return true;
			}
		}
	}

	return false;
}

void arrange (vector<Tile*>& tiles) {
	for (int i = 0; i < tiles.size(); i++) {
		for (int j = i+1; j < tiles.size(); j++) {
			tiles[i]->match(tiles[j]);
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

		size_t count = 0;
		bitset<TILE_SIZE>* cam_arr = new bitset<TILE_SIZE>[TILE_SIZE];

		bitset<TILE_SIZE>* edges = new bitset<TILE_SIZE>[4];
		bitset<TILE_SIZE> l_edge;
		bitset<TILE_SIZE> r_edge;

		while(count < TILE_SIZE) {
			string cam_arr_row;
			getline(input_stream, cam_arr_row);

			assert(cam_arr_row.size() == TILE_SIZE);

			bitset<TILE_SIZE> row;
			for (int idx = 0; idx < TILE_SIZE; idx++) {
				row[TILE_SIZE-1-idx] = cam_arr_row[idx] == '#';
			};
			
			l_edge[TILE_SIZE-1-count] = cam_arr_row[0] == '#';		
			r_edge[TILE_SIZE-1-count] = cam_arr_row[TILE_SIZE-1] == '#';
			if (count == 0) {edges[0] = row;}
			else if (count == TILE_SIZE-1) {edges[2] = row;}

			cam_arr[count] = row;	
			count++;
		};

		edges[1] = r_edge;
		edges[3] = l_edge;
		tiles.push_back(new Tile(tile_id, cam_arr, edges));
	}

	arrange(tiles);

	// Part 1
	long unsigned int p1_ans = 1;
	vector<Tile*>::const_iterator tIt = tiles.begin();
	for (; tIt != tiles.end(); tIt++) {
		p1_ans *= ((*tIt)->get_num_matched() == 2) ? (*tIt)->tile_id : 1;
	}
	cout << "[p1]: " << p1_ans << endl;

	return 0;
}
