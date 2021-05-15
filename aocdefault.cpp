#include <aocdefault.h>

using namespace std;
namespace fs = std::filesystem;

// Assumption
// 1. The basename of the input .txt will always be the same as the source base
// name
// 2. Binaries will be executed in the context of the parent directory of bin
// dir

const std::string RUN_MODE = "run";

std::string get_input_file_path(int argc, char* argv[]) {
        fs::path bin_path = argv[0];
        std::string mode;
        if (argc == 2) {
                mode = argv[1];
        }

        if (mode.empty()) {
                cout << "err: execution mode is missing!" << endl;
                exit(1);
        };

        return mode == RUN_MODE
                   ? "./" + bin_path.stem().string() + ".txt"
                   : "./" + bin_path.stem().string() + "_" + mode + ".txt";
}

std::ifstream get_input_stream(int argc, char* argv[]) {
        fs::path input_file_path = get_input_file_path(argc, argv);
        if (!fs::exists(input_file_path)) {
                cout << "err: input file path does not exist!" << endl;
                exit(1);
        }

        ifstream input_stream(input_file_path);
        if (!input_stream.is_open()) {
                cout << "err: input stream is not open!" << endl;
                exit(1);
        }

        return input_stream;
}
