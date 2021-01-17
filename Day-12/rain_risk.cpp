#include <iostream>
#include <stdio.h>
#include <algorithm>
#include <vector>
#include <fstream>
#include <math.h>
#include <unordered_map>
#include <aocdefault.h>

#define PI 3.14159265

using namespace std;

const char FORWARD = 'F'; 
const char TURN_LEFT = 'L';
const char TURN_RIGHT = 'R';
const char COMPASS[4] = {'N', 'E', 'S', 'W'};

double deg_to_rad (double deg) { return deg*PI/180; }
double delta_v (double bearing, int delta) { return cos(deg_to_rad(bearing))*delta; }
double delta_h (double bearing, int delta) { return sin(deg_to_rad(bearing))*delta; }
double find_manhattan_dist (double x_val, double y_val) { return abs(x_val) + abs(y_val); }
char find_axis_pair (char d) { 
	int start_d_idx = find(COMPASS, COMPASS + 4, d) - COMPASS;
	return COMPASS[(start_d_idx + 1)%(sizeof(COMPASS)/sizeof(*COMPASS))]; 
}

/*
 * b: rotation angle
 * 	(+)b: clockwise rotation
 * 	(-)b: anti-clockwise rotation 
 */
pair<double, double> rotate (double x_val, double y_val, int b) {
	double cosb = cos(deg_to_rad(b));
	double sinb = sin(deg_to_rad(b));

	return make_pair(cosb*x_val + sinb*y_val, cosb*y_val - sinb*x_val);
}

void process_action (const char x, double &x_val, const char y, double &y_val, double &bearing, const char action, const int a_delta) {
	if (action == FORWARD) {
		y_val += delta_v(bearing, a_delta);
		x_val += delta_h(bearing, a_delta);
	} else if (action == TURN_LEFT) {
		bearing -= a_delta;
	} else if (action == TURN_RIGHT) {
		bearing += a_delta;
	} else {
		if (action == x) { x_val += a_delta; }
		else if (action == y) { y_val += a_delta; }
		else {
			int d_idx = find(COMPASS, COMPASS + 4, action) - COMPASS;
			int d_inv = (d_idx + 2)%4;
			if (COMPASS[d_inv] == x) {
				x_val -= a_delta;
			} else if (COMPASS[d_inv] == y) {
				y_val -= a_delta;
			}
		}
	}
}

void update_waypoint (const char x, double &wp_x_val, const char y, double &wp_y_val, const char action, const int a_delta) {
	if (action == TURN_LEFT) {
		pair<double, double> rotated_coord = rotate(wp_x_val, wp_y_val, a_delta*-1);
		wp_x_val = rotated_coord.first;
		wp_y_val = rotated_coord.second;
	} else if (action == TURN_RIGHT) {
		pair<double, double> rotated_coord = rotate(wp_x_val, wp_y_val, a_delta);
		wp_x_val = rotated_coord.first;
		wp_y_val = rotated_coord.second;
	} else {
		if (action == x) { wp_x_val += a_delta; }
		else if (action == y) { wp_y_val += a_delta; }
		else {
			int d_idx = find(COMPASS, COMPASS + 4, action) - COMPASS;
			int d_inv = (d_idx + 2)%4;
			if (COMPASS[d_inv] == x) {
				wp_x_val -= a_delta;
			} else if (COMPASS[d_inv] == y) {
				wp_y_val -= a_delta;
			}
		}
	}
}


int main (int argc, char *argv[]) {
	ifstream input_stream(get_input_file_path(argc, argv));	
	
	string line;
	if (input_stream.is_open()) {
		// Part 1
		char y = 'E';
		char x = find_axis_pair(y);
		double y_val = 0;
		double x_val = 0;
		double bearing = 0;
		while (getline(input_stream, line)) {
			char action = line[0];
			int a_delta = stoi(line.substr(1));
			process_action(x, x_val, y, y_val, bearing, action, a_delta);
		}	
		printf("\tP1 Manhattan Distance: %d\n", (int)(find_manhattan_dist(x_val, y_val) + 0.5));

		//reset
		y = 'N';
		x = find_axis_pair(y);
		y_val = 0;
		x_val = 0;
		bearing = 0;
		input_stream.clear();
		input_stream.seekg(0, ios::beg);

		// Part 2
		double wp_x_val = 10;
		double wp_y_val = 1;
		while (getline(input_stream, line)) {
			char action = line[0];
			int a_delta = stoi(line.substr(1));
			if (action == FORWARD) {
				int count = 0;
				while (count < a_delta) {
					x_val += wp_x_val;
					y_val += wp_y_val;
					count++;
				}
			} else {
				update_waypoint(x, wp_x_val, y, wp_y_val, action, a_delta);
			}
		}
		printf("\tP2 Manhattan Distance: %d\n", (int)(find_manhattan_dist(x_val, y_val) + 0.5));
	}

	return 1;
}
