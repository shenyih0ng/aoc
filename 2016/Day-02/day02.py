import sys
from typing import Union, Final

move_sets: list[str] = sys.stdin.read().strip().split("\n")

Coord2D = tuple[int, int]

GRID_2: Final[list[list[Union[int, str]]]] = [
    [""] * 2 + [1] + [""] * 2,
    [""] + [2,3,4] + [""],
    [5,6,7,8,9],
    [""] + ['A','B','C'] + [""],
    [""] * 2 + ['D'] + [""] * 2,
]

def make_move_p1(curr_pos: Coord2D, delta: Coord2D) -> Coord2D:
    new_pos = (curr_pos[0] + delta[0], curr_pos[1] + delta[1])
    # check if valid (in 3x3 grid)
    if (new_pos[0] < 0 or 
        new_pos[0] > 2 or 
        new_pos[1] < 0 or 
        new_pos[1] > 2):
        return curr_pos
    return new_pos

def make_move_p2(curr_pos: Coord2D, delta: Coord2D) -> Coord2D:
    new_pos = (curr_pos[0] + delta[0], curr_pos[1] + delta[1])
    # check if valid (in GRID_2)
    if (new_pos[0] < 0 or 
        new_pos[0] > 4 or 
        new_pos[1] < 0 or 
        new_pos[1] > 4 or 
        GRID_2[new_pos[1]][new_pos[0]] == "") :
        return curr_pos
    return new_pos
 
code_p1: str = ""
code_p2: str = ""

pos_p1: tuple[int, int] = (1, 1)
pos_p2: tuple[int, int] = (0, 2)
for moves in move_sets:
    for m in moves:
        delta: Coord2D = (0,0)
        if m == "R":
            delta = (1, 0)
        elif m == "L":
            delta = (-1, 0)
        elif m == "U":
            delta = (0, -1)
        elif m == "D":
            delta = (0, 1)

        pos_p1 = make_move_p1(pos_p1, delta)
        pos_p2 = make_move_p2(pos_p2, delta)

    code_p1 += str(pos_p1[1] * 3 + pos_p1[0] + 1)
    code_p2 += str(GRID_2[pos_p2[1]][pos_p2[0]])

print(f"Part 1: {code_p1}")
print(f"Part 2: {code_p2}")
 