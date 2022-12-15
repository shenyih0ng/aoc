import sys
from itertools import product
from typing import Tuple, Optional

SAND_START: Tuple[int, int] = (0, 500)


def gen_range(start: int, end: int) -> range:
    return range(start, end + 1) if (start < end) else range(end, start + 1)


def get_pos(coord: Tuple[int, int], grid, floor: Optional[int] = None):
    if (floor is not None) and coord[0] == floor:
        return '#'
    if coord[0] in grid:
        return grid[coord[0]].get(coord[1])
    grid[coord[0]] = {}
    return None


def drop(grid, pos: Tuple[int, int], floor: Optional[int] = None) -> Optional[Tuple[int, int]]:
    if get_pos(pos, grid, floor) is not None:
        return None

    max_row = max(grid.keys()) if floor is None else floor
    for i in range(1, max_row - pos[0] + 1):
        if get_pos((i + pos[0], pos[1]), grid, floor) is None:
            continue
        return (pos[0] + (i - 1), pos[1])

    return None


def stablize(grid, pos: Tuple[int, int], floor: Optional[int] = None) -> Optional[Tuple[int, int]]:
    dl_coord: Tuple[int, int] = (pos[0] + 1, pos[1] - 1)  # down left
    dr_coord: Tuple[int, int] = (pos[0] + 1, pos[1] + 1)  # down right

    if get_pos(dl_coord, grid, floor) is None:
        dl_drop = drop(grid, dl_coord, floor)
        if dl_drop is None:
            return None
        return stablize(grid, dl_drop, floor)
    elif get_pos(dr_coord, grid, floor) is None:
        dr_drop = drop(grid, dr_coord, floor)
        if dr_drop is None:
            return None
        return stablize(grid, dr_drop, floor)
    else:
        grid[pos[0]][pos[1]] = 'o'
        return pos


input = map(lambda x: x.split(" -> "), sys.stdin.read().strip().split("\n"))

grid_p1 = dict()
grid_p2 = dict()

for coords in input:
    for c1, c2 in zip(coords[:-1], coords[1:]):
        y1, x1 = map(int, c1.split(','))
        y2, x2 = map(int, c2.split(','))
        for x, y in product(gen_range(x1, x2), gen_range(y1, y2)):
            if x not in grid_p1:
                grid_p1[x] = {}
                grid_p2[x] = {}
            grid_p1[x][y] = "#"
            grid_p2[x][y] = "#"

# Part 1
count_p1 = 0
floor = max(grid_p1.keys()) + 2
while True:
    initial_drop = drop(grid_p1, SAND_START)
    assert initial_drop

    if not stablize(grid_p1, initial_drop):
        break
    count_p1 += 1

# Part 2
count_p2 = 0
floor = max(grid_p2.keys()) + 2
while True:
    initial_drop = drop(grid_p2, SAND_START, floor)
    if initial_drop is None:
        break

    assert stablize(grid_p2, initial_drop, floor)
    count_p2 += 1

print(f"Part 1: {count_p1}")
print(f"Part 2: {count_p2}")
