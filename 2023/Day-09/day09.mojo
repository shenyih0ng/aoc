from sys import argv


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer


fn get_next(int_vec: DynamicVector[Int]) -> Int:
    var is_end: Bool = True
    for i in range(len(int_vec)):
        if int_vec[i] == 0: continue
        is_end = False
        break

    if is_end: return 0

    var new_int_vec = DynamicVector[Int]()
    for i in range(len(int_vec) - 1):
        new_int_vec.append(int_vec[i + 1] - int_vec[i])
    return get_next(new_int_vec) + int_vec[len(int_vec) - 1]


fn main() raises:
    let input = read(argv()[1])
    let lines_str = input.split("\n")

    var val_sum_p1: Int = 0
    var val_sum_p2: Int = 0

    for i in range(len(lines_str) - 1):
        let nums_str = lines_str[i].split(" ")
        var int_vec_p1 = DynamicVector[Int]()
        var int_vec_p2 = DynamicVector[Int]()

        for j in range(len(nums_str)):
            int_vec_p1.append(atol(nums_str[j]))
            int_vec_p2.append(atol(nums_str[len(nums_str) - 1 - j]))

        val_sum_p1 += get_next(int_vec_p1)
        val_sum_p2 += get_next(int_vec_p2)

    print("Part 1:", val_sum_p1)
    print("Part 2:", val_sum_p2)
