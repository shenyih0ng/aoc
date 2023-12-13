from sys import argv


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()[:-1]
    file.close()
    return buffer


fn find_vert_reflect(pattern: DynamicVector[String], part2: Bool = False) -> Int:
    let _num_cols = len(pattern[0])

    for i in range(0, _num_cols - 1):
        var left_idx = i
        var right_idx = i + 1
        var diff: Int = 0
        while left_idx >= 0 and right_idx < _num_cols:
            for j in range(len(pattern)):
                if pattern[j][left_idx] != pattern[j][right_idx]: diff += 1
            left_idx -= 1
            right_idx += 1

        if (diff == 0 and not part2) or (diff == 1 and part2):
            return i + 1

    return -1


fn find_hori_reflect(pattern: DynamicVector[String], part2: Bool = False) -> Int:
    let _num_rows = len(pattern)

    for i in range(0, _num_rows - 1):
        var top_idx = i
        var btm_idx = i + 1
        var diff: Int = 0
        while top_idx >= 0 and btm_idx < _num_rows:
            for j in range(len(pattern[0])):
                if pattern[top_idx][j] != pattern[btm_idx][j]: diff += 1
            top_idx -= 1
            btm_idx += 1
        if (diff == 0 and not part2) or (diff == 1 and part2):
            return i + 1

    return -1


fn main() raises:
    let input = read(argv()[1])
    let patterns_str = input.split("\n\n")

    var summary_p1: Int = 0
    var summary_p2: Int = 0

    for i in range(len(patterns_str)):
        let pattern = patterns_str[i].split("\n")
        let vert_reflect_idx_p1 = find_vert_reflect(pattern)
        let vert_reflect_idx_p2 = find_vert_reflect(pattern, True)
        summary_p1 += (
            vert_reflect_idx_p1 if vert_reflect_idx_p1
            != -1 else find_hori_reflect(pattern) * 100
        )
        summary_p2 += (
            vert_reflect_idx_p2 if vert_reflect_idx_p2
            != -1 else find_hori_reflect(pattern, True) * 100
        )

    print("Part 1:", summary_p1)
    print("Part 2:", summary_p2)
