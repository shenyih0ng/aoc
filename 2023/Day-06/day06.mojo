from sys import argv


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer


fn to_int_vec(str_vec: DynamicVector[String]) raises -> DynamicVector[Int]:
    var out = DynamicVector[Int]()
    for i in range(len(str_vec)):
        if str_vec[i] == "":
            continue
        out.push_back(atol(str_vec[i]))
    return out


fn combine(str_vec: DynamicVector[String]) raises -> Int:
    var combined_str: String = ""
    for i in range(len(str_vec)):
        if str_vec[i] == "":
            continue
        combined_str += str_vec[i]
    return atol(combined_str)


fn find_num_ways(time: Int, dist: Int) -> Int:
    var front: Int = 0
    while (time - front) * front <= dist:
        front += 1

    var back: Int = time
    while (time - back) * back <= dist:
        back -= 1

    return back - front + 1


fn main() raises:
    let races_str: String = read(argv()[1])
    let races_str_split = races_str.split("\n")

    let time_vec_str = races_str_split[0][5:].split(" ")
    let dist_vec_str = races_str_split[1][9:].split(" ")

    let time_vec: DynamicVector[Int] = to_int_vec(time_vec_str)
    let dist_vec: DynamicVector[Int] = to_int_vec(dist_vec_str)

    let combined_time: Int = combine(time_vec_str)
    let combined_dist: Int = combine(dist_vec_str)

    debug_assert(
        len(time_vec) == len(dist_vec), "time_vec and dist_vec have different lengths!"
    )

    var total: Int = 1
    for i in range(len(time_vec)):
        total *= find_num_ways(time_vec[i], dist_vec[i])

    print("Part 1:", total)
    print("Part 2:", find_num_ways(combined_time, combined_dist))
