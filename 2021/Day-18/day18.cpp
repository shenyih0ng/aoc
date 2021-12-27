#include <fstream>
#include <iostream>
#include <numeric>
#include <vector>

struct Node {
    int val = -1;
    Node *parent = nullptr;
    Node *left = nullptr;
    Node *right = nullptr;
    Node(int val, Node *parent, Node *left, Node *right)
        : val(val), parent(parent), left(left), right(right) {}
};

Node *build(std::string &math_hw) {
    if (math_hw[0] == '[') {
        math_hw.erase(0, 1);
        Node *left = build(math_hw);
        math_hw.erase(0, 1); // remove ,
        Node *right = build(math_hw);
        math_hw.erase(0, 1); // remove ]
        Node *root = new Node(-1, nullptr, left, right);
        left->parent = root;
        right->parent = root;
        return root;
    } else {
        int operand_len = isdigit(math_hw[0]) && isdigit(math_hw[1]) ? 2 : 1;
        int val = std::stoi(math_hw.substr(0, operand_len));
        math_hw.erase(0, operand_len);
        return new Node(val, nullptr, nullptr, nullptr);
    }
}

Node *find_leftmost_node(Node *node) {
    if (node->val != -1) {
        return node;
    } else if (node->left != nullptr) {
        return find_leftmost_node(node->left);
    } else {
        return find_leftmost_node(node->right);
    }
}

Node *find_rightmost_node(Node *node) {
    if (node->val != -1) {
        return node;
    } else if (node->right != nullptr) {
        return find_rightmost_node(node->right);
    } else {
        return find_rightmost_node(node->left);
    }
}

Node *find_right(Node *node, Node *prev) {
    if (node == prev) {
        return find_right(node->parent, node);
    }

    if (node->right != nullptr && node->right != prev) {
        return find_leftmost_node(node->right);
    } else if (node->parent == nullptr) {
        return nullptr;
    } else {
        return find_right(node->parent, node);
    }

    return nullptr;
}

Node *find_left(Node *node, Node *prev) {
    if (node == prev) {
        return find_left(node->parent, node);
    }

    if (node->left != nullptr && node->left != prev) {
        return find_rightmost_node(node->left);
    } else if (node->parent == nullptr) {
        return nullptr;
    } else {
        return find_left(node->parent, node);
    }

    return nullptr;
}

bool explode(Node *node, int nest) {
    if (node->val == -1 && nest == 4) {
        // find left node
        Node *left_node = find_left(node, node);
        if (left_node != nullptr) {
            left_node->val += node->left->val;
        }
        // find right node
        Node *right_node = find_right(node, node);
        if (right_node != nullptr) {
            right_node->val += node->right->val;
        }
        node->val = 0;
        node->left = nullptr;
        node->right = nullptr;

        return true;
    }

    return false;
}

bool split(Node *node) {
    if (node->val >= 10) {
        node->left =
            new Node(floor(float(node->val) / 2), node, nullptr, nullptr);
        node->right =
            new Node(ceil(float(node->val) / 2), node, nullptr, nullptr);
        node->val = -1;

        return true;
    }

    return false;
}

bool reduce_inorder(Node *curr_node, bool to_explode, int nest = 0) {
    if (curr_node->left &&
        reduce_inorder(curr_node->left, to_explode, nest + 1)) {
        return true;
    }

    if (to_explode ? explode(curr_node, nest) : split(curr_node)) {
        return true;
    }

    if (curr_node->right &&
        reduce_inorder(curr_node->right, to_explode, nest + 1)) {
        return true;
    }

    return false;
}

void reduce(Node *root) {
    bool done = false;
    while (!done) {
        while (reduce_inorder(root, true)) {
        }
        done = !reduce_inorder(root, false);
    }
}

Node *add(Node *n1, Node *n2) {
    Node *newRoot = new Node(-1, nullptr, n1, n2);
    n1->parent = newRoot;
    n2->parent = newRoot;
    return newRoot;
}

int eval(Node *curr_node) {
    if (curr_node->val != -1) {
        return curr_node->val;
    } else {
        return (3 * eval(curr_node->left)) + (2 * eval(curr_node->right));
    }
}

int find_max_sum(std::vector<std::string> &math_hw_vect) {
    int max_sum = -1;
    for (int i = 0; i < math_hw_vect.size() - 1; ++i) {
        for (int j = i + 1; j < math_hw_vect.size(); ++j) {
            std::string i_math_hw = math_hw_vect[i];
            std::string j_math_hw = math_hw_vect[j];
            Node *ij_combi = add(build(i_math_hw), build(j_math_hw));
            reduce(ij_combi);
            std::string i_math_hw_copy = math_hw_vect[i];
            std::string j_math_hw_copy = math_hw_vect[j];
            Node *ji_combi = add(build(j_math_hw_copy), build(i_math_hw_copy));
            reduce(ji_combi);
            max_sum =
                std::max(max_sum, std::max(eval(ij_combi), eval(ji_combi)));
        }
    }

    return max_sum;
}

int main(int argc, char *argv[]) {
    std::ifstream input_stream("./input.txt");
    std::string math_hw;

    std::vector<std::string> math_hw_vect;
    Node *prev = nullptr;
    while (getline(input_stream, math_hw)) {
        math_hw_vect.push_back(math_hw);
        if (prev == nullptr) {
            prev = build(math_hw);
        } else {
            Node *to_add = build(math_hw);
            Node *added = add(prev, to_add);
            reduce(added);
            prev = added;
        }
    }

    std::cout << "Part 1: " << eval(prev) << std::endl;
    std::cout << "Part 2: " << find_max_sum(math_hw_vect) << std::endl;

    return -1;
}
