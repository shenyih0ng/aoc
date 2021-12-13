#include <fstream>
#include <iostream>
#include <map>
#include <vector>

struct Node {
    std::string name;
    std::vector<Node *> conns;
};

bool is_small_cave(std::string cave) {
    return std::find_if(cave.begin(), cave.end(),
                        [](char c) { return !islower(c); }) == cave.end();
}

bool exists(std::vector<std::string> &path, std::string node_name) {
    return std::find(path.begin(), path.end(), node_name) != path.end();
}

int get_num_path_no_repeated_small(Node curr_node,
                                   std::vector<std::string> path = {}) {
    if (is_small_cave(curr_node.name) && exists(path, curr_node.name)) {
        return 0;
    } else if (curr_node.name == "end") {
        return 1;
    } else {
        int num_path = 0;
        path.push_back(curr_node.name);
        std::vector<Node *>::iterator nodeIt = curr_node.conns.begin();
        for (; nodeIt != curr_node.conns.end(); ++nodeIt) {
            num_path += get_num_path_no_repeated_small(**nodeIt, path);
        }

        return num_path;
    }
}

int get_num_path_one_repeated_small(Node curr_node,
                                    std::vector<std::string> path = {},
                                    bool contains_repeated_small = false) {
    if (is_small_cave(curr_node.name) && exists(path, curr_node.name) &&
        contains_repeated_small) {
        return 0;
    } else if (curr_node.name == "end") {
        return 1;
    } else {
        int total_path = 0;
        if (is_small_cave(curr_node.name) && exists(path, curr_node.name) &&
            !contains_repeated_small) {
            contains_repeated_small = true;
        }
        path.push_back(curr_node.name);
        std::vector<Node *>::iterator nodeIt = curr_node.conns.begin();
        for (; nodeIt != curr_node.conns.end(); ++nodeIt) {
            total_path += get_num_path_one_repeated_small(
                **nodeIt, path, contains_repeated_small);
        }

        return total_path;
    }
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string line;

    std::map<std::string, Node> nodes_map;
    while (getline(input_stream, line)) {
        std::string from = line.substr(0, line.find('-'));
        std::string to = line.substr(line.find('-') + 1);
        if (nodes_map.find(from) == nodes_map.end()) {
            Node from_node = {from};
            nodes_map[from] = from_node;
        }
        if (nodes_map.find(to) == nodes_map.end()) {
            Node to_node = {to};
            nodes_map[to] = to_node;
        }

        if ((from != "start" && from != "end") &&
            (to != "start" && to != "end")) {
            nodes_map[from].conns.push_back(&nodes_map[to]);
            nodes_map[to].conns.push_back(&nodes_map[from]);
        } else if (from == "start" || to == "end") {
            nodes_map[from].conns.push_back(&nodes_map[to]);
        } else if (to == "start" || from == "end") {
            nodes_map[to].conns.push_back(&nodes_map[from]);
        }
    }

    std::cout << "Part 1: "
              << get_num_path_no_repeated_small(nodes_map["start"])
              << std::endl;
    std::cout << "Part 2: "
              << get_num_path_one_repeated_small(nodes_map["start"])
              << std::endl;

    return -1;
}