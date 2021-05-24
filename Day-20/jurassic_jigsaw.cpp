#include <assert.h>
#include <algorithm>
#include <aocdefault.h>

#define TILE_SIZE 10

using namespace std;

// Image Transformations
// - rotate: 90 deg clockwise
// - flip: h-flip

vector<string> _rotate_img (vector<string> img) {
	vector<string> rotated(img[0].size());

	for (int c = 0; c < img[0].size(); c++) {
		string new_row;
		for (int r = 0; r < img.size(); r++) {
			new_row += img[img.size()-1-r][c];
		}
		rotated[c] = new_row;
	}

	return rotated;
}
vector<string> _rotate_img_n_times (int num_rotation, vector<string> img) {
	vector<string> curr_img = img;
	for (int count = 0; count < num_rotation; count++) {
		curr_img = _rotate_img(curr_img);
	}

	return curr_img;
}

vector<string> _flip_img (vector<string> img) {
	for (int idx = 0; idx < img.size(); idx++) {
		reverse(img[idx].begin(), img[idx].end());
	}

	return img;
}

// Border Transformations
// - rotate: 90 deg clockwise
// - flip: h-flip

vector<string> _rotate_borders (vector<string> b) {
	vector<string> rotated(b.size());
	
	reverse(b[3].begin(), b[3].end());
	reverse(b[1].begin(), b[1].end());
	
	for (int idx = 0; idx < 4; idx++) {
		rotated[(idx+1)%4] = b[idx];
	}

	return rotated;
}

vector<string> _flip_borders (vector<string> b) {
	vector<string> flipped(b.size());
	
	reverse(b[0].begin(), b[0].end());	
	flipped[0] = b[0];

	reverse(b[2].begin(), b[2].end());	
	flipped[2] = b[2];
	
	flipped[1] = b[3];
	flipped[3] = b[1];

	return flipped;
}

// Tile Class

class Tile {
		int id;
		vector<string> image;
		vector<string> borders;
		
		Tile* matches[4] = {nullptr};

		public:
			Tile (int tile_id) { id = tile_id; }

			int get_id () { return id; }

			vector<string> get_image() { return image; } 

			void set_image (vector<string> img_arr) { image = img_arr; }
	
			vector<string> get_borders() { return borders; } 

			void set_borders (vector<string> borders_arr) { borders = borders_arr; }

			int get_num_sides_matched () { 
				return 4-count(begin(matches), end(matches), nullptr);
			}

			void set_matched_side (int side_idx, Tile* t) { 
				matches[side_idx] = t;
			}

			bool matched_with (Tile* t) {
				return find(begin(matches), end(matches), t) != end(matches);
			}

			Tile** get_matches() { return matches; }

			bool match (Tile*);

			void find_match (vector<Tile*>&);

			void transform_image (int num_rotation, bool flipped) {
				image = _rotate_img_n_times(num_rotation%4, image);
				if (flipped) {
					image = _flip_img(image);
				}
			}
			
			friend ostream& operator<< (ostream& out, Tile* t) {
					out << "id: " << t->get_id() << endl;
					vector<string> image = t->get_image();
					for (int _i = 0; _i < image.size(); _i++) {
						cout << image[_i] << endl;
					}
				
					out << "borders: " << t->get_borders() << endl;
					out << "no. sides matched: " << t->get_num_sides_matched() << endl;
	
					return out;
			};
};

bool is_ul_tile(Tile* t) {
	Tile** matches = t->get_matches();
	return matches[0] == nullptr && matches[3] == nullptr;
}

// Returns the border idx of b1 that matches
// - border idx of b2 can derived by (border idx of b1 +2) % 4
// - -1 if there is no match
int match_borders (vector<string> b1, vector<string> b2) {
	for (int bIdx = 0; bIdx < 4; bIdx++) {
		if (b1[bIdx] == b2[(bIdx+2)%4]) {
			return bIdx;
		}
	}

	return -1;
}

bool Tile::match(Tile* o_tile) {
	vector<string> t1_borders = get_borders();
	vector<string> t2_borders = o_tile->get_borders();

	bool matched = false;

	int num_rotation = 0;
	bool flipped = false;

	for (int num_check = 0; num_check < 4; num_check++) {
		// op1: rotation
		t2_borders = _rotate_borders(t2_borders);
		num_rotation++;
		int match_idx_1 = match_borders(t1_borders, t2_borders);
		if (match_idx_1 != -1) {
			this->set_matched_side(match_idx_1, o_tile);

			o_tile->set_borders(t2_borders);		
			o_tile->set_matched_side((match_idx_1+2)%4, this);
			o_tile->transform_image(num_rotation, flipped);

			matched = true;
			break;
		}

		// op2: flip
		vector<string> t2_borders_flipped = _flip_borders(t2_borders);
		flipped = true;
		int match_idx_2 = match_borders(t1_borders, t2_borders_flipped);
		if (match_idx_2 != -1) {
			this->set_matched_side(match_idx_2, o_tile);

			o_tile->set_borders(t2_borders_flipped);		
			o_tile->set_matched_side((match_idx_2+2)%4, this);
			o_tile->transform_image(num_rotation, flipped);

			matched = true;
			break;
		}
		
		flipped = false;
	}

	return matched;
}

void Tile::find_match (vector<Tile*>& tiles) {
	for (int idx = 0; idx < tiles.size(); idx++) {
		Tile* t = tiles[idx];
		if (t->get_id() != get_id() && t->get_num_sides_matched() < 4
			&& !matched_with(t) && match(t)) {
			t->find_match(tiles);
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
		vector<string> image;
		vector<string> borders(4);

		while(count < TILE_SIZE) {
			string tile_row;
			getline(input_stream, tile_row);
			
			switch (count) {
				case 0:
					borders[0] = tile_row;
					break;
				case TILE_SIZE-1:
					borders[2] = tile_row;
					break;		
			}

			image.push_back(tile_row.substr(1, tile_row.size()-2));	
			borders[1] += tile_row[tile_row.size()-1];
			borders[3] += tile_row[0];

			count++;
		};

		tile->set_image(image);
		tile->set_borders(borders);

		tiles.push_back(tile);	
	};
		
	tiles[0]->find_match(tiles); // use first tile as anchor
	Tile* ul_tile = nullptr;
	
	// Part 1
	long unsigned int p1_ans = 1;
	for (int idx = 0; idx < tiles.size(); idx++) {
		Tile* t = tiles[idx];
		if (is_ul_tile(t)) {
			ul_tile = t;
		}
		p1_ans *= t->get_num_sides_matched() == 2 ? t->get_id() : 1;
	}

	cout << "[p1]: " << p1_ans << endl;
}
