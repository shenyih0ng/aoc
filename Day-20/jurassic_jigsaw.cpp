#include <cmath>
#include <assert.h>
#include <algorithm>
#include <aocdefault.h>

#define TILE_SIZE 10
#define IMAGE_SIZE 8

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

			int get_num_sides_matched () { return 4-count(begin(matches), end(matches), nullptr); }

			void set_matched_side (int side_idx, Tile* t) { matches[side_idx] = t; }

			bool matched_with (Tile* t) { return find(begin(matches), end(matches), t) != end(matches); }

			Tile** get_matches() { return matches; }

			char get_pixel(int h_offset, int v_offset) { return image[v_offset][h_offset]; };

			void transform_image (int num_rotation, bool flipped) {
				image = _rotate_img_n_times(num_rotation%4, image);
				if (flipped) {
					image = _flip_img(image);
				}
			}

			bool match (Tile*);

			void find_match (vector<Tile*>&);

			Tile* find_tile(int, int);
			
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

bool Tile::match(Tile* o_tile) {
	vector<string> t1_borders = get_borders();
	vector<string> t2_borders = o_tile->get_borders();

	int matched_border_idx = -1; // t1_border idx that matched

	int num_rotation = 0;
	bool flipped = false;

	for (int num_check = 0; num_check < 4; num_check++) {
		// op1: rotation
		t2_borders = _rotate_borders(t2_borders);
		num_rotation++;
		matched_border_idx = match_borders(t1_borders, t2_borders);
		if (matched_border_idx != -1) { 
			break; 
		}

		// op2: flip
		vector<string> t2_borders_flipped = _flip_borders(t2_borders);
		flipped = true;
		matched_border_idx = match_borders(t1_borders, t2_borders_flipped);
		if (matched_border_idx != -1) {
			t2_borders = t2_borders_flipped;
			break;
		}
		
		flipped = false;
	}

	if (matched_border_idx != -1) {
		this->set_matched_side(matched_border_idx, o_tile);

		o_tile->set_borders(t2_borders); // update border after transformation		
		o_tile->set_matched_side((matched_border_idx+2)%4, this);
		o_tile->transform_image(num_rotation, flipped);

		return true;
	}

	return false;
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

// Find tile using tile offset
Tile* Tile::find_tile (int h_offset, int v_offset) {
	if (h_offset == 0 && v_offset == 0) {
		return this;
	} else if (h_offset == 0) {
		return matches[2] == nullptr 
			? nullptr 
			: matches[2]->find_tile(0, v_offset-1);
	} else if (v_offset == 0) {
		return matches[1] == nullptr 
			? nullptr 
			: matches[1]->find_tile(h_offset-1, 0);
	}

	return matches[2] == nullptr 
		? nullptr 
		: matches[2]->find_tile(h_offset, v_offset-1);
}

class Dragon {
	public:
		int width;
		int height;
		
		// relative to top left (0,0)
		// +x -> right
		// +y -> down
		vector<pair<int, int>> coordinates;

		Dragon (vector<pair<int, int>> coords, int w, int h) :
			coordinates { coords },
			width { w },
			height { h }
		{}
		
		// 90 deg clockwise rotate	
		void rotate () {
			vector<pair<int, int>>::iterator coordIt = coordinates.begin();
			for (; coordIt != coordinates.end(); coordIt++) {
				pair<int, int> curr_coord = *coordIt;
				coordIt->second = curr_coord.first; // x1 = y2
				coordIt->first = height-1-curr_coord.second; // flipped y1 = x2
			}

			int _temp = width;
			width = height;
			height = _temp;
		};
		
		// h-flip
		void flip() {
			vector<pair<int, int>>::iterator coordIt = coordinates.begin();
			for (; coordIt != coordinates.end(); coordIt++) {
				coordIt->first = width-1-coordIt->first; // flipped x1 = x2	
			}
		}

		friend ostream& operator<< (ostream& out, Dragon* d) {
			out << "w: " << d->width << " h: " << d->height << endl;
			vector<pair<int, int>> coords = d->coordinates;
			
			string display (d->height*d->width, ' ');
	
			vector<pair<int, int>>::const_iterator cIt = coords.begin();
			for (; cIt != coords.end(); cIt++) {
				display[(cIt->second*d->width) + cIt->first] = '#';
			}
			
			for (int i = 0; i < d->height; i++) {
				cout << display.substr(i*d->width, d->width) << endl;
			}

			return out;
		}
};

void find_dragon (Dragon* dragon, Tile* ul_tile, set<pair<int, int>>& occupied_by_dragon) {
	vector<pair<int, int>> dragon_coords = dragon->coordinates;

	int dragon_y_offset = 0;
	bool end = false;
	while(!end) {
		int dragon_x_offset = 0;
		bool row_end = false;
		while(!row_end) {
			bool found = true;
			set<pair<int, int>> dragon_occupied;

			for (int idx = 0; idx < dragon_coords.size(); idx++) {
				pair<int, int> coord = dragon_coords[idx];
				Tile* target_tile = ul_tile->find_tile(
					(dragon_x_offset + coord.first) / IMAGE_SIZE,
					(dragon_y_offset + coord.second) / IMAGE_SIZE
				);
				char target_px = target_tile->get_pixel(
					(dragon_x_offset + coord.first) % IMAGE_SIZE,
					(dragon_y_offset + coord.second) % IMAGE_SIZE
				);

				found = target_px == '#';
				if (found == false) { 
					dragon_occupied.clear();
					break; 
				}
				dragon_occupied.insert(make_pair(dragon_x_offset + coord.first, dragon_y_offset + coord.second));
			}

			occupied_by_dragon.insert(dragon_occupied.begin(), dragon_occupied.end());

			dragon_x_offset++;
			row_end = ul_tile->find_tile((dragon_x_offset+dragon->width-1)/IMAGE_SIZE, 0) == nullptr;
		}

		dragon_y_offset++;
		end = ul_tile->find_tile(0, (dragon_y_offset+dragon->height-1)/IMAGE_SIZE) == nullptr;
	}
}

int main (int argc, char* argv[]) {
	ifstream input_stream = get_input_stream(argc, argv);
	
	vector<Tile*> tiles;
	int total_non_empty_pixels = 0; // keep track of number of non-empty pixels in image

	string line;
	while(getline(input_stream, line)) {
		if (line.find("Tile") == string::npos) { continue; }

		size_t div_pos = line.find_first_of(' ');
		int tile_id = stoi(line.substr(div_pos + 1, line.size()-div_pos + 2));

		Tile* tile = new Tile(tile_id);

		size_t num_lines = 0;
		vector<string> image;
		vector<string> borders(4);

		while(num_lines < TILE_SIZE) {
			string tile_row;
			getline(input_stream, tile_row);
			
			switch (num_lines) {
				case 0:
					borders[0] = tile_row;
					break;
				case TILE_SIZE-1:
					borders[2] = tile_row;
					break;
				default:
					string image_row = tile_row.substr(1, tile_row.size()-2);
					image.push_back(image_row);	
					total_non_empty_pixels += count(begin(image_row), end(image_row), '#');
			}

			borders[1] += tile_row[tile_row.size()-1];
			borders[3] += tile_row[0];

			num_lines++;
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
	
	// Parse dragon
	const string dragon[3] = {
		"                  # ",
		"#    ##    ##    ###",
		" #  #  #  #  #  #   "
	};

	vector<pair<int, int>> dragon_coords;	
	for (int r = 0; r < 3; r++) {
		for (int c = 0; c < dragon[0].size(); c++) {
			if (dragon[r][c] == '#') {
				dragon_coords.push_back(make_pair(c,r));
			}
		}			
	}

	Dragon* dragon_obj = new Dragon(dragon_coords, dragon[0].size(), 3);

	set<pair<int, int>> occupied_by_dragon;
	for (int i = 0; i < 4; i++) {
		dragon_obj->rotate();
		find_dragon(dragon_obj, ul_tile, occupied_by_dragon);		
		dragon_obj->flip();
		find_dragon(dragon_obj, ul_tile, occupied_by_dragon);

		dragon_obj->flip();
	}
	
	cout << "[p2]: " << total_non_empty_pixels - occupied_by_dragon.size() << endl;
	
	return 0;
}

