from sys import argv

alias PART2_NUM_ITERS = 1000000000

fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()[:-1]
    file.close()
    return buffer


fn tilt_north(inout grid: DynamicVector[String], num_rows: Int, num_cols: Int):
    for c in range(num_cols):
        for r in range(num_rows):
            if grid[r * num_cols + c] != "O": continue
            var curr_r: Int = r - 1
            while (
                curr_r >= 0
                and grid[curr_r * num_cols + c] != "#"
                and grid[curr_r * num_cols + c] != "O"
            ):
                curr_r -= 1
            if curr_r + 1 != r:
                grid[(curr_r + 1) * num_cols + c] = "O"
                grid[r * num_cols + c] = "."


fn rotate(
    grid: DynamicVector[String], num_rows: Int, num_cols: Int
) -> DynamicVector[String]:
    var new_grid = DynamicVector[String]()
    new_grid.resize(num_rows * num_cols, -1)
    for r in range(num_rows):
        for c in range(num_cols):
            new_grid[c * num_rows + (num_rows - r - 1)] = grid[r * num_cols + c]
    return new_grid


fn get_north_load(grid: DynamicVector[String], num_rows: Int, num_cols: Int) -> Int:
    var total: Int = 0
    for r in range(num_rows):
        for c in range(num_cols):
            if grid[r * num_cols + c] != "O":
                continue
            total += num_rows - r
    return total


@value
struct Grid(CollectionElement):
    var grid: DynamicVector[String]
    var num_rows: Int
    var num_cols: Int

    fn tilt(self) -> Self:
        var new_grid = self.grid
        tilt_north(new_grid, self.num_rows, self.num_cols)
        return Grid(new_grid, self.num_rows, self.num_cols)

    fn rotate(self) -> Self:
        let new_grid = rotate(self.grid, self.num_rows, self.num_cols)
        return Grid(new_grid, self.num_rows, self.num_cols)

    fn __eq__(self: Grid, other: Self) -> Bool:
        for i in range(len(self.grid)):
            if self.grid[i] != other.grid[i]:
                return False
        return True


fn main() raises:
    let input = read(argv()[1])[:-1]
    var grid = DynamicVector[String]()

    let split = input.split("\n")
    let num_rows = len(split)
    let num_cols = len(split[0])

    for r in range(num_rows):
        for c in range(num_cols):
            grid.push_back(split[r][c])

    let grid_p2 = grid

    tilt_north(grid, num_rows, num_cols)
    print("Part 1:", get_north_load(grid, num_rows, num_cols))

    var cache = DynamicVector[Grid]()
    var curr_grid = Grid(grid_p2, num_rows, num_cols)

    var iter: Int = 0
    while iter < PART2_NUM_ITERS:
        iter += 1
        for _ in range(4):
            curr_grid = curr_grid.tilt()
            curr_grid = curr_grid.rotate()

        for i in range(len(cache)):
            if cache[i] == curr_grid:
                let diff = (iter - 1) - i
                iter += ((PART2_NUM_ITERS - iter) // diff) * diff

        cache.push_back(curr_grid)

    print("Part 2:", get_north_load(curr_grid.grid, num_rows, num_cols))
