#include <assert.h>
#include <fstream>
#include <iostream>
#include <math.h>
#include <vector>

typedef std::vector<std::string> image_t;

image_t create_blank_image(int num_row, int num_col) {
    image_t blank_image;
    for (int row = 0; row < num_row; row++) {
        std::string blank_row(num_col, '.');
        blank_image.push_back(blank_row);
    }

    return blank_image;
}

// pad image vector by 1 in all axis
void pad_image(image_t &curr_image, char pad_char) {
    for (int idx = 0; idx < curr_image.size(); idx++) {
        curr_image[idx] = pad_char + curr_image[idx] + pad_char;
    }
    std::string padded_row(curr_image[0].size(), pad_char);
    curr_image.insert(curr_image.begin(), padded_row);
    curr_image.push_back(padded_row);
}

int get_algo_key(int rIdx, int cIdx, const image_t &image, char default_val) {
    int deltas[3] = {-1, 0, 1};
    int multiplier = 8;
    int algo_key = 0;
    for (int rDeltaIdx = 0; rDeltaIdx < 3; rDeltaIdx++) {
        for (int cDeltaIdx = 0; cDeltaIdx < 3; cDeltaIdx++) {
            char adj_val = default_val;
            int adj_rIdx = rIdx + deltas[rDeltaIdx];
            int adj_cIdx = cIdx + deltas[cDeltaIdx];
            if (!(adj_rIdx < 0 || adj_rIdx >= image.size() || adj_cIdx < 0 ||
                  adj_cIdx >= image[0].size())) {
                adj_val = image[adj_rIdx][adj_cIdx];
            }
            algo_key += (adj_val == '#' ? 1 : 0) * pow(2, multiplier);
            multiplier--;
        }
    }

    return algo_key;
}

int64_t enhance(image_t curr_image, const std::string img_algo, int num_enhance,
                char default_val) {
    int num_rows = curr_image.size();
    int num_cols = curr_image[0].size();

    int64_t num_lit = 0;
    image_t enhanced_image = create_blank_image(num_rows, num_cols);
    for (int rIdx = 0; rIdx < num_rows; rIdx++) {
        for (int cIdx = 0; cIdx < num_cols; cIdx++) {
            enhanced_image[rIdx][cIdx] =
                img_algo[get_algo_key(rIdx, cIdx, curr_image, default_val)];
            if (num_enhance == 1) {
                num_lit += enhanced_image[rIdx][cIdx] == '#';
            }
        }
    }

    char next_default_val = default_val == '.' ? img_algo[0] : img_algo[511];
    pad_image(enhanced_image, next_default_val);

    return num_enhance == 1 ? num_lit
                            : enhance(enhanced_image, img_algo, num_enhance - 1,
                                      next_default_val);
}

int64_t enhance(image_t image, const std::string img_algo, int num_enhance) {
    pad_image(image, '.');
    return enhance(image, img_algo, num_enhance, '.');
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");

    std::string img_algo; // length of 512
    getline(input_stream, img_algo);
    assert(img_algo.size() == 512);

    image_t input_image;
    std::string input_image_line;
    getline(input_stream, input_image_line); // ignore divider
    while (getline(input_stream, input_image_line)) {
        input_image.push_back(input_image_line);
    }

    std::cout << "Part 1: " << enhance(input_image, img_algo, 2) << std::endl;
    std::cout << "Part 2: " << enhance(input_image, img_algo, 50) << std::endl;

    return EXIT_SUCCESS;
}
