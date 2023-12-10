from sys import argv


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer

struct Grid:
    var buffer: String
    var row_len: Int

    fn __init__(inout self, buffer: String, row_len: Int):
        self.buffer = buffer
        self.row_len = row_len

    fn get_pos(self, pos: Int, dr: Int, dc: Int) -> Int:
        let row: Int = pos // self.row_len
        let col: Int = pos % self.row_len
        return (row + dr) * self.row_len + (col + dc)


fn get_next(grid: Grid, curr_pos: Int, prev_pos: Int) -> Int:
    let c: String = grid.buffer[curr_pos]

    var p1: Int = -1
    var p2: Int = -1
    if c == "|":
        p1 = grid.get_pos(curr_pos, -1, 0)
        p2 = grid.get_pos(curr_pos, 1, 0)
    if c == "-":
        p1 = grid.get_pos(curr_pos, 0, -1)
        p2 = grid.get_pos(curr_pos, 0, 1)
    if c == "7":
        p1 = grid.get_pos(curr_pos, 0, -1)
        p2 = grid.get_pos(curr_pos, 1, 0)
    if c == "J":
        p1 = grid.get_pos(curr_pos, -1, 0)
        p2 = grid.get_pos(curr_pos, 0, -1)
    if c == "L":
        p1 = grid.get_pos(curr_pos, 0, 1)
        p2 = grid.get_pos(curr_pos, -1, 0)
    if c == "F":
        p1 = grid.get_pos(curr_pos, 0, 1)
        p2 = grid.get_pos(curr_pos, 1, 0)

    if p1 == -1 and p2 == -1:
        return -1
    return p1 if p1 != prev_pos else p2


fn get_loop(grid: Grid, start_pos: Int) -> DynamicVector[Int]:
    var loop: DynamicVector[Int] = DynamicVector[Int]()
    loop.push_back(start_pos)

    var curr_pos = grid.get_pos(start_pos, -1, 0) # note: by observation
    var prev_pos = start_pos
    while curr_pos != start_pos:
        loop.push_back(curr_pos)
        let temp = curr_pos
        curr_pos = get_next(grid, curr_pos, prev_pos)
        prev_pos = temp

    return loop

fn num_tiles_in_loop(grid: Grid, start_c: String, loop: DynamicVector[Int]) -> Int:
    var count: Int = 0
    for r in range(len(grid.buffer) // grid.row_len):
        var inside: Bool = False
        for c in range(grid.row_len):
            let pos = grid.get_pos(0, r, c)

            var part_of_loop: Bool = False
            for i in range(len(loop)):
                if loop[i] != pos: continue
                part_of_loop = True
                break

            let pos_c: String = grid.buffer[pos] if pos != loop[0] else start_c

            if part_of_loop and (pos_c == "|" or pos_c == "F" or pos_c == "7"):
                # can also check for "L" and "J" as a alternative to "F" and "7"
                inside = not inside
            
            if inside and not part_of_loop:
                count += 1
    
    return count


fn main() raises:
    let input = read(argv()[1])
    var row_len: Int = 0
    while input[row_len] != "\n":
        row_len += 1
    row_len += 1  # include the newline

    var start_pos: Int = -1
    for i in range(len(input)):
        if input[i] != "S": continue
        start_pos = i
        break

    let grid = Grid(input, row_len)
    let loop = get_loop(grid, start_pos)
    print("Part 1:", len(loop) // 2)

    let loop_start = loop[0]
    let loop_end = loop[len(loop) - 1]

    let start_c: String = "J" # note: by observation

    print("Part 2:", num_tiles_in_loop(grid, "J", loop))
