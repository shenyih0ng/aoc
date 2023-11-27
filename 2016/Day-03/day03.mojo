from utils.vector import DynamicVector


fn read(fname: String) raises -> String:
    var f: FileHandle = open(fname, "r")
    let data: String = f.read()[:-1]  # remove trailing whitespace
    f.close()
    return data


fn parse(data: String) raises -> DynamicVector[Int]:
    var out: DynamicVector[Int] = DynamicVector[Int]()

    var i: Int = 0
    let length: Int = len(data)

    while i < length:
        while data[i] == " " or data[i] == "\n":
            i += 1

        var j: Int = i + 1
        while j < length and data[j] != " " and data[j] != "\n":
            j += 1

        out.push_back(atol(data[i:j]))
        i = j

    return out


fn transpose(vec: DynamicVector[Int]) -> DynamicVector[Int]:
    debug_assert(len(vec) % 3 == 0, "input vec should have length divisible by 3")

    var vec_t: DynamicVector[Int] = DynamicVector[Int](len(vec))
    vec_t.resize(len(vec))

    let m: Int = 3
    let n: Int = len(vec) // m

    for i in range(n):
        for j in range(m):
            vec_t[j * n + i] = vec[i * m + j]

    return vec_t


fn num_valid_triangles(tri_lengths: DynamicVector[Int]) -> Int:
    var num_valid: Int = 0
    let length = len(tri_lengths)

    for i in range(0, len(tri_lengths), 3):
        let l1: Int = tri_lengths[i]
        let l2: Int = tri_lengths[i + 1]
        let l3: Int = tri_lengths[i + 2]

        if l1 + l2 > l3 and l1 + l3 > l2 and l2 + l3 > l1:
            num_valid += 1

    return num_valid


fn main() raises:
    let data: String = read("input.txt")

    let tri_lengths: DynamicVector[Int] = parse(data)
    let num_valid: Int = num_valid_triangles(tri_lengths)
    print("Part 1:", num_valid)

    let tri_lengths_t: DynamicVector[Int] = transpose(tri_lengths)
    let num_valid_t: Int = num_valid_triangles(tri_lengths_t)
    print("Part 2:", num_valid_t)
