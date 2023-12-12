from sys import argv
from python import Python, Dictionary


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer


fn in_dict(d: Dictionary, key: Tuple[Int, Int, Int]) raises -> Bool:
    return not Python.is_type(d.get(key), Python.none())


fn get_num_ways(
    arr: String,
    cond: DynamicVector[Int],
    a_idx: Int,
    cond_idx: Int,
    acc: Int,
    memo: Dictionary,
    _part2: Bool,
    _choices: DynamicVector[String],
) raises -> Int:
    let key = (a_idx, cond_idx, acc)
    if in_dict(memo, key):
        return memo.get((a_idx, cond_idx, acc)).to_float64().to_int()

    let cond_len = len(cond) * (5 if _part2 else 1)
    let cond_idx_mod = cond_idx % len(cond)

    if a_idx >= len(arr):
        if cond_idx == cond_len - 1 and acc == cond[cond_idx_mod]:
            return 1
        elif cond_idx == cond_len and acc == 0:
            return 1
        return 0

    var num_ways: Int = 0
    for c_idx in range(len(_choices)):
        if arr[a_idx] == "?" or arr[a_idx] == _choices[c_idx]:
            if _choices[c_idx] == "#":
                num_ways += get_num_ways(
                    arr, cond, a_idx + 1, cond_idx, acc + 1, memo, _part2, _choices
                )
            else:
                if acc == 0:
                    num_ways += get_num_ways(
                        arr, cond, a_idx + 1, cond_idx, 0, memo, _part2, _choices
                    )
                elif cond_idx < cond_len and cond[cond_idx_mod] == acc:
                    num_ways += get_num_ways(
                        arr, cond, a_idx + 1, cond_idx + 1, 0, memo, _part2, _choices
                    )

    if not in_dict(memo, key):
        memo[key] = num_ways

    return num_ways


fn main() raises:
    let input = read(argv()[1])
    let arrangements_str = input[:-1].split("\n")

    var _choices = DynamicVector[String](2)
    _choices.push_back(".")
    _choices.push_back("#")

    var total_count_p1: Int = 0
    var total_count_p2: Int = 0

    for i in range(len(arrangements_str)):
        let _split = arrangements_str[i].split(" ")
        let conds = _split[1].split(",")

        var conds_int = DynamicVector[Int]()
        for j in range(len(conds)):
            conds_int.push_back(atol(conds[j]))

        let count_p1 = get_num_ways(
            _split[0], conds_int, 0, 0, 0, Python.dict(), False, _choices
        )
        total_count_p1 += count_p1

        var p2_arr: String = ""
        for _i in range(5):
            p2_arr += _split[0]
            if _i != 4: p2_arr += "?"

        let count_p2 = get_num_ways(
            p2_arr, conds_int, 0, 0, 0, Python.dict(), True, _choices
        )
        total_count_p2 += count_p2

    print("Part 1:", total_count_p1)
    print("Part 2:", total_count_p2)
