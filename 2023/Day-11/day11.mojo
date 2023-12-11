from sys import argv
from math import min, max


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer


def repeat(c: String, n: Int) -> String:
    var result: String = ""
    for i in range(n): result += c
    return result

@value
struct Point(CollectionElement):
    var r: Int
    var c: Int

struct Grid:
    var rows: DynamicVector[String]
    var empty_rows: DynamicVector[Int]
    var empty_cols: DynamicVector[Int]
    var points: DynamicVector[Point]

    fn __init__(inout self, owned rows: DynamicVector[String]) raises:
        self.rows = rows

        let _num_cols = len(self.rows[0])
        self.empty_rows = DynamicVector[Int](len(rows))
        self.empty_cols = DynamicVector[Int](_num_cols)
        self.points = DynamicVector[Point]()

        let empty_row: String = repeat('.', len(rows[0]))
        for i in range(len(rows)):
            if empty_row == self.rows[i]: self.empty_rows.push_back(i)
        
        for j in range(_num_cols):
            var is_empty: Bool = True
            for i in range(len(rows)):
                if self.rows[i][j] == '#': 
                    self.points.push_back(Point(i, j))
                    is_empty = False
            if is_empty: self.empty_cols.push_back(j)
    
    fn is_empty_row(self, row: Int) -> Bool:
        for i in range(len(self.empty_rows)):
            if self.empty_rows[i] == row: return True
        return False

    fn is_empty_col(self, col: Int) -> Bool:
        for i in range(len(self.empty_cols)):
            if self.empty_cols[i] == col: return True
        return False


fn main() raises:
    let input = read(argv()[1])

    let grid = Grid(input.split("\n"))

    var total_len_p1: Int = 0
    var total_len_p2: Int = 0
    for p1_idx in range(len(grid.points)):
        for p2_idx in range(p1_idx + 1, len(grid.points)):
            let p1 = grid.points[p1_idx]
            let p2 = grid.points[p2_idx]

            var dist_p1: Int = 0
            var dist_p2: Int = 0
            for r in range(min(p1.r, p2.r), max(p1.r, p2.r)):
                dist_p1 += 2 if grid.is_empty_row(r) else 1
                dist_p2 += 1000000 if grid.is_empty_row(r) else 1

            for c in range(min(p1.c, p2.c), max(p1.c, p2.c)):
                dist_p1 += 2 if grid.is_empty_col(c) else 1
                dist_p2 += 1000000 if grid.is_empty_col(c) else 1

            total_len_p1 += dist_p1
            total_len_p2 += dist_p2
    
    print("Part 1:", total_len_p1)
    print("Part 2:", total_len_p2)
 