#include <fstream>
#include <iostream>
#include <regex>
#include <vector>

#define NUM_TEASPOONS 100
#define NUM_CALORIES 500

using namespace std;

typedef long long int ll;

struct Properties {
  ll capacity;
  ll durability;
  ll flavor;
  ll texture;
  ll calories;
};

struct Ingredient {
  string name;
  Properties props;
};

ll calc_score(Properties& props) {
  return max(props.capacity, 0LL) * max(props.durability, 0LL) *
         max(props.flavor, 0LL) * max(props.texture, 0LL);
}

ll get_highest_score(bool check_calories, int idx,
                     vector<Ingredient>& ingredients, int teaspoons_left,
                     Properties props) {
  if ((idx >= ingredients.size()) || (teaspoons_left <= 0))
    return check_calories
               ? (props.calories == NUM_CALORIES ? calc_score(props) : 0)
               : calc_score(props);

  ll highest_score = 0;

  Ingredient i = ingredients[idx];
  for (int ts = 1; ts <= teaspoons_left; ts++) {
    Properties new_props = props;
    new_props.capacity += i.props.capacity * ts;
    new_props.durability += i.props.durability * ts;
    new_props.flavor += i.props.flavor * ts;
    new_props.texture += i.props.texture * ts;
    new_props.calories += i.props.calories * ts;

    highest_score = max(get_highest_score(check_calories, idx + 1, ingredients,
                                          teaspoons_left - ts, new_props),
                        highest_score);
  }

  return highest_score;
}

int main(int argc, char* argv[]) {
  ifstream input_stream("./input.txt");
  string line;

  regex re(
      "(\\w+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), "
      "texture (-?\\d+), calories (-?\\d+)");

  vector<Ingredient> ingredients;
  while (getline(input_stream, line)) {
    smatch match;
    regex_match(line, match, re);

    ingredients.push_back((Ingredient){
        match[1],
        (Properties){stoll(match[2]), stoll(match[3]), stoll(match[4]),
                     stoll(match[5]), stoll(match[6])}});
  }

  cout << "Part 1: "
       << get_highest_score(false, 0, ingredients, NUM_TEASPOONS,
                            (Properties){0, 0, 0, 0, 0})
       << endl;

  cout << "Part 2: "
       << get_highest_score(true, 0, ingredients, NUM_TEASPOONS,
                            (Properties){0, 0, 0, 0, 0})
       << endl;

  return EXIT_SUCCESS;
}
