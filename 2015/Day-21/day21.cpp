#include <fstream>
#include <iostream>

using namespace std;

struct Character {
  int health;
  int damage;
  int armor;
};

enum ItemTypes { WEAPON, ARMOR, RING };

struct Item {
  int cost;
  int damage;
  int armor;
};

using Loadout = Item;

const Item weapons[5] = {
    {8, 4, 0}, {10, 5, 0}, {25, 6, 0}, {40, 7, 0}, {74, 8, 0},
};

const Item armors[6] = {
    {0, 0, 0}, {13, 0, 1}, {31, 0, 2}, {53, 0, 3}, {75, 0, 4}, {102, 0, 5},
};

const Item rings[8] = {
    {0, 0, 0},   {0, 0, 0},  {25, 1, 0}, {50, 2, 0},
    {100, 3, 0}, {20, 0, 1}, {40, 0, 2}, {80, 0, 3},
};

bool play(Character& player, Character& enemy) {
  return player.health / max(enemy.damage - player.armor, 1) >=
         enemy.health / max(player.damage - enemy.armor, 1);
}

vector<Loadout> get_all_loadouts() {
  vector<Loadout> loadouts;

  for (int w = 0; w < 5; w++) {
    Item weapon = weapons[w];
    for (int a = 0; a < 6; a++) {
      Item armor = armors[a];
      for (int r1 = 0; r1 < 8; r1++) {
        Item ring_1 = rings[r1];
        for (int r2 = r1 + 1; r2 < 8; r2++) {
          if (r1 == r2) continue;
          Item ring_2 = rings[r2];

          int cost = weapon.cost + armor.cost + ring_1.cost + ring_2.cost;
          int total_damage = weapon.damage + ring_1.damage + ring_2.damage;
          int total_armor = armor.armor + ring_1.armor + ring_2.armor;

          loadouts.push_back({cost, total_damage, total_armor});
        }
      }
    }
  }

  return loadouts;
}

int find_min_max_cost(Character& player, Character& enemy,
                      const vector<Loadout>& loadouts, bool find_min) {
  int cost = find_min ? INT_MAX : INT_MIN;

  for (auto loadout : loadouts) {
    player.damage = loadout.damage;
    player.armor = loadout.armor;

    bool player_wins = play(player, enemy);
    if (player_wins && find_min) {
      cost = min(cost, loadout.cost);
    } else if (!player_wins && !find_min) {
      cost = max(cost, loadout.cost);
    }
  }

  return cost;
}

int main(int argc, char* argv[]) {
  ifstream input_stream(argv[1]);

  string e_health, e_damage, e_armor;

  getline(input_stream, e_health);
  getline(input_stream, e_damage);
  getline(input_stream, e_armor);

  Character player = {100, 0, 0};
  Character enemy = {stoi(e_health.substr(e_health.find(":") + 1)),
                     stoi(e_damage.substr(e_damage.find(":") + 1)),
                     stoi(e_armor.substr(e_armor.find(":") + 1))};

  const vector<Loadout> loadouts = get_all_loadouts();

  cout << "Part 1: " << find_min_max_cost(player, enemy, loadouts, true)
       << endl;
  cout << "Part 2: " << find_min_max_cost(player, enemy, loadouts, false)
       << endl;

  return EXIT_SUCCESS;
}
