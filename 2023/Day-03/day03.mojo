from sys import argv
from utils.vector import DynamicVector


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer = file.read()
    file.close()
    return buffer


fn solution(schematic: String) raises -> Tuple[Int, Int]:
    fn has_visited(visited: DynamicVector[Int], pos: Int) -> Bool:
        var idx: Int = 0
        while idx < len(visited):
            if visited[idx] == pos:
                return True
            idx += 1
        return False

    var part_sum: Int = 0
    var gear_ratio_sum: Int = 0

    var col_idx: Int = 0
    while schematic[col_idx] != "\n":
        col_idx += 1
    let num_col: Int = col_idx + 1  # including `\n` delimiter

    let s_ptr = schematic._as_ptr()
    var visited = DynamicVector[Int]()

    for row in range(len(schematic) // num_col):
        for col in range(num_col - 1):
            let c = s_ptr.load(row * num_col + col)
            if isdigit(c) or c == ord("."):
                continue

            var _num_adj: Int = 0
            var _gear_ratio: Int = 1

            for dr in range(-1, 2, 1):
                for dc in range(-1, 2, 1):
                    if dr == 0 and dc == 0:
                        continue

                    let idx = (row + dr) * num_col + (col + dc)
                    let a_c = s_ptr.load(idx)
                    if not isdigit(a_c):
                        continue

                    # get left end
                    var dleft: Int = 0
                    while isdigit(s_ptr.load(idx + dleft)):
                        dleft -= 1

                    # get right end
                    var dright: Int = 0
                    while isdigit(s_ptr.load(idx + dright)):
                        dright += 1

                    let l_num_idx: Int = idx + dleft + 1
                    let r_num_idx: Int = idx + dright

                    # check if it is visited
                    if has_visited(visited, l_num_idx) or has_visited(
                        visited, r_num_idx
                    ):
                        continue

                    visited.push_back(l_num_idx)
                    visited.push_back(r_num_idx)

                    let num: Int = atol(schematic[l_num_idx:r_num_idx])

                    _num_adj += 1
                    _gear_ratio *= num

                    part_sum += num

            if _num_adj != 2:
                continue
            gear_ratio_sum += _gear_ratio

    return (part_sum, gear_ratio_sum)


fn main() raises:
    let schematic: String = read(argv()[1])

    let sol = solution(schematic)
    print("Part 1:", sol.get[0, Int]())
    print("Part 2:", sol.get[1, Int]())
