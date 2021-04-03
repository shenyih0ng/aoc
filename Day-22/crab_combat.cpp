#include <aocdefault.h>

using namespace std;

bool has_winner (vector<int>& p1, vector<int>& p2) {
	return p1.size() == 0 || p2.size() == 0;
}

int calc_winner_score(vector<int>& p1, vector<int>& p2) {
	vector<int>& winner = (p1.size() != 0) ? p1 : p2;
	int size = winner.size();

	int score = 0;
	for (; size > 0; size--) {
		score += winner[winner.size()-size] * size;
	}

	return score;
}

void play (vector<int>& p1, vector<int>& p2) {
	while (!has_winner(p1, p2)) {
		int p1_c = p1[0];
		p1.erase(p1.begin());
		int p2_c = p2[0];
		p2.erase(p2.begin());

		if (p1_c > p2_c) {
			p1.push_back(p1_c);
			p1.push_back(p2_c);
		} else {
			p2.push_back(p2_c);
			p2.push_back(p1_c);
		}
	}
}

int main (int argc, char* argv[]) {
	ifstream input_stream(get_input_file_path(argc, argv));

	if (input_stream.is_open()){
		string line;

		vector<int> p1;
		vector<int> p2;
		bool _p2 = false;
		while(getline(input_stream, line)) {
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
		play(p1, p2);
		cout << "wscore: " << calc_winner_score(p1, p2) << endl;
	}

	return 0;
}
