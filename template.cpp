#include <fstream>
#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {
	ifstream input_stream(argv[1]);
	string line;

	while (getline(input_stream, line)) {
		cout << line << endl;
	}

	return EXIT_SUCCESS;
}

