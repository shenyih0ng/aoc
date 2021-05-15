#include <assert.h>
#include <algorithm>
#include <map>

#include <aocdefault.h>

using namespace std;

// TODO using vector as a key for memo/cache of repeated hands seems to be a
// dumb idea
// - can i perhaps reduce the execution time with strings instead (strings seems
// to be the way to go)
// - edit seen hands to use strings instead (tuple of strings/pair of strings)
// instead of using 2 seperate cache to keep track of THINK SIMPLE!

int calc_score(vector<int>& p) {
        int score = 0;
        for (int size = p.size(); size > 0; size--) {
                score += p[p.size() - size] * size;
        }

        return score;
}

bool has_winner(vector<int>& p1, vector<int>& p2) {
        return p1.size() == 0 || p2.size() == 0;
}

bool if_subgame(vector<int>& p1, vector<int>& p2) {
        return ((p1.size() - 1) >= p1[0]) && ((p2.size() - 1) >= p2[0]);
}

bool if_repeated_hand(vector<int>& p1, vector<int>& p2,
                      vector<vector<int>>& prev_p1,
                      vector<vector<int>>& prev_p2) {
        assert(prev_p1.size() == prev_p2.size());

        vector<vector<int>>::iterator p1_found =
            find(prev_p1.begin(), prev_p1.end(), p1);
        vector<vector<int>>::iterator p2_found =
            find(prev_p2.begin(), prev_p2.end(), p2);
        if (p1_found != prev_p1.end() && p2_found != prev_p2.end()) {
                return distance(p1_found, prev_p1.begin()) ==
                       distance(p2_found, prev_p2.begin());
        }

        return false;
}

// w recursive combat
bool play(vector<int>& p1, vector<int>& p2, vector<vector<int>>& prev_p1,
          vector<vector<int>>& prev_p2) {
        while (!has_winner(p1, p2) &&
               !if_repeated_hand(p1, p2, prev_p1, prev_p2)) {
                prev_p1.push_back(p1);
                prev_p2.push_back(p2);

                bool p1_round_win;
                if (if_subgame(p1, p2)) {
                        vector<int> sub_p1;
                        copy(p1.begin() + 1, p1.begin() + p1[0] + 1,
                             back_inserter(sub_p1));
                        vector<int> sub_p2;
                        copy(p2.begin() + 1, p2.begin() + p2[0] + 1,
                             back_inserter(sub_p2));

                        vector<vector<int>> sub_prev_p1, sub_prev_p2;
                        p1_round_win =
                            play(sub_p1, sub_p2, sub_prev_p1, sub_prev_p2);
                } else {
                        p1_round_win = p1[0] > p2[0];
                }

                if (p1_round_win) {
                        p1.push_back(p1[0]);
                        p1.push_back(p2[0]);
                } else {
                        p2.push_back(p2[0]);
                        p2.push_back(p1[0]);
                }
                p1.erase(p1.begin());
                p2.erase(p2.begin());
        }

        if (!has_winner(p1, p2)) {
                return true;
        }

        return (p1.size() != 0);
}

// w/o recursive combat
bool play(vector<int>& p1, vector<int>& p2) {
        while (!has_winner(p1, p2)) {
                if (p1[0] > p2[0]) {
                        p1.push_back(p1[0]);
                        p1.push_back(p2[0]);
                } else {
                        p2.push_back(p2[0]);
                        p2.push_back(p1[0]);
                }
                p1.erase(p1.begin());
                p2.erase(p2.begin());
        }

        return (p1.size() != 0);
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        if (input_stream.is_open()) {
                string line;

                vector<int> p1;
                vector<int> p2;
                bool _p2 = false;
                while (getline(input_stream, line)) {
                        if ((int)line[0] >= 48 && (int)line[0] <= 57) {
                                if (!_p2) {
                                        p1.push_back(atoi(line.c_str()));
                                } else {
                                        p2.push_back(atoi(line.c_str()));
                                }
                        } else if ((int)line[0] == 0) {
                                _p2 = true;
                        }
                };
                vector<int> p1_copy = p1;
                vector<int> p2_copy = p2;

                // Part 1
                bool part1_p1_win = play(p1, p2);
                cout << "[part1]" << endl;
                cout << ((part1_p1_win) ? "p1 wins!" : "p2 wins!");
                cout << " wscore: " << calc_score((part1_p1_win) ? p1 : p2)
                     << endl;

                // Part 2
                vector<vector<int>> prev_p1;
                vector<vector<int>> prev_p2;
                bool part2_p1_win = play(p1_copy, p2_copy, prev_p1, prev_p2);
                cout << "[part2]" << endl;
                cout << ((part2_p1_win) ? "p1 wins!" : "p2 wins!");
                cout << " wscore: "
                     << calc_score((part2_p1_win) ? p1_copy : p2_copy) << endl;
        }

        return 0;
}
