#include <assert.h>
#include <map>

#include <aocdefault.h>

using namespace std;

#define NUM_CYCLES 6

typedef vector<int> Pos;

void _get_neighbours(Pos og_pos, vector<Pos>& neighbours, Pos curr_pos,
                     size_t dim) {
        if (dim <= 0 && curr_pos != og_pos) {
                neighbours.push_back(curr_pos);
        } else if (dim > 0) {
                for (int dd = -1; dd <= 1; dd++) {
                        Pos u_pos = curr_pos;
                        u_pos[dim - 1] += dd;
                        _get_neighbours(og_pos, neighbours, u_pos, dim - 1);
                }
        }
}

vector<Pos> get_neighbours(Pos pos) {
        vector<Pos> neighbours;
        _get_neighbours(pos, neighbours, pos, pos.size());

        return neighbours;
}

size_t get_num_active(map<Pos, bool>& mem, vector<Pos>& neighbours) {
        size_t num_active = 0;
        vector<Pos>::const_iterator vIt = neighbours.begin();
        for (; vIt != neighbours.end(); vIt++) {
                map<Pos, bool>::const_iterator found = mem.find(*vIt);
                num_active += found != mem.end() && found->second;
        }

        return num_active;
}

bool get_next_pos_state(Pos& pos, vector<Pos>& neighbours,
                        map<Pos, bool>& mem) {
        bool curr_active = mem.find(pos) != mem.end();
        size_t num_active = get_num_active(mem, neighbours);
        if (curr_active) {
                return num_active == 2 || num_active == 3;
        } else {
                return num_active == 3;
        }
}

map<Pos, bool> run_cycle(map<Pos, bool>& mem) {
        map<Pos, bool> next_state;

        map<Pos, bool>::const_iterator mIt = mem.begin();
        for (; mIt != mem.end(); mIt++) {
                Pos pos = mIt->first;
                vector<Pos> neighbours = get_neighbours(pos);
                if (get_next_pos_state(pos, neighbours, mem)) {
                        next_state.insert(make_pair(pos, true));
                };

                vector<Pos>::const_iterator vIt = neighbours.begin();
                for (; vIt != neighbours.end(); vIt++) {
                        Pos n_pos = *vIt;
                        if (mem.find(n_pos) == mem.end()) {
                                vector<Pos> n_neighbours =
                                    get_neighbours(n_pos);
                                if (get_next_pos_state(n_pos, n_neighbours,
                                                       mem)) {
                                        next_state.insert(
                                            make_pair(n_pos, true));
                                }
                        }
                }
        }

        return next_state;
}

int run_n_cycles(map<Pos, bool> sState, size_t n) {
        size_t count = 0;
        map<Pos, bool> cState = sState;
        while (count < NUM_CYCLES) {
                cState = run_cycle(cState);
                count++;
        }

        return cState.size();
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        if (input_stream.is_open()) {
                map<Pos, bool> start_state;
                string row;
                int rIdx = 0;
                while (getline(input_stream, row)) {
                        for (int cIdx = 0; cIdx < row.size(); cIdx++) {
                                if (row[cIdx] == '#') {
                                        Pos pos{cIdx, rIdx, 0};
                                        start_state.insert(make_pair(pos, 1));
                                }
                        }
                        rIdx++;
                }

                // Part 1
                cout << "[p1]: " << run_n_cycles(start_state, NUM_CYCLES)
                     << endl;

                // Part 2
                map<Pos, bool> n_start_state;
                map<Pos, bool>::iterator mIt = start_state.begin();
                for (; mIt != start_state.end(); mIt++) {
                        Pos pos = mIt->first;
                        pos.push_back(0);
                        n_start_state.insert(make_pair(pos, 1));
                }
                cout << "[p2]: " << run_n_cycles(n_start_state, NUM_CYCLES)
                     << endl;
        }

        return 0;
}
