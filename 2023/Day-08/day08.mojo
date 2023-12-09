from sys import argv
from math import lcm


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer


@value
struct Node(CollectionElement):
    var name: String
    var id: Int
    var left: Int
    var right: Int


fn get_node_id(node_ids: DynamicVector[String], name: String) -> Int:
    for i in range(len(node_ids)):
        if node_ids[i] == name: return i
    return -1


fn get_moves(
    nodes: DynamicVector[Node],
    inst_str: String,
    start: Int,
    end_predicate: fn (Node) -> Bool,
) -> Int:
    var inst_idx: Int = 0
    var moves: Int = 0
    var curr_node = nodes[start]
    while not end_predicate(curr_node):
        let inst = inst_str[inst_idx]

        if inst == "L":
            curr_node = nodes[curr_node.left]
        elif inst == "R":
            curr_node = nodes[curr_node.right]

        inst_idx = (inst_idx + 1) % len(inst_str)
        moves += 1
    return moves


fn is_ZZZ(node: Node) -> Bool:
    return node.name == "ZZZ"


fn end_with_Z(node: Node) -> Bool:
    return node.name[2] == "Z"


fn main() raises:
    let input = read(argv()[1])
    let inst_nodes_str = input.split("\n\n")

    let inst_str = inst_nodes_str[0]
    let nodes_str = inst_nodes_str[1].split("\n")

    var node_ids = DynamicVector[String](len(nodes_str))
    for i in range(len(nodes_str)):
        if nodes_str[i] == "": continue
        node_ids.push_back(nodes_str[i].split(" ")[0])

    var nodes = DynamicVector[Node](len(nodes_str))
    for i in range(len(nodes_str)):
        if nodes_str[i] == "": continue
        let split = nodes_str[i].split(" ")
        nodes.push_back(
            Node(
                split[0],
                i,
                get_node_id(node_ids, split[2][1:-1]),
                get_node_id(node_ids, split[3][:-1]),
            )
        )

    print("Part 1:", get_moves(nodes, inst_str, get_node_id(node_ids, "AAA"), is_ZZZ))

    var node_moves = DynamicVector[Int]()
    for i in range(len(nodes)):
        if nodes[i].name[2] != "A": continue
        node_moves.push_back(get_moves(nodes, inst_str, i, end_with_Z))

    var lcm_moves: Int = 1
    for i in range(len(node_moves)):
        lcm_moves = lcm(lcm_moves, node_moves[i])

    print("Part 2:", lcm_moves)
