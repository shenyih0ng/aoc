import re
import sys
from typing import Tuple

BLANK_CELL: str = ' '
DELTAS: list[Tuple[int, int]] = [(0, 1),  (1, 0), (0, -1),  (-1, 0)]

[grid_str, dir_str] = sys.stdin.read().split("\n\n")
grid_str = grid_str.split('\n')

grid_width = max(map(len, grid_str))
grid: list[str] = list(
    map(lambda row: (row + BLANK_CELL * (grid_width - len(row))), grid_str))
cube_size = min(map(lambda x: len(x.strip()), grid_str))


def move(pos: Tuple[int, int], delta: Tuple[int, int]) -> Tuple[int, int]:
    return ((pos[0] + delta[0]) % len(grid), (pos[1] + delta[1]) % grid_width)


def get_password(pos: Tuple[int, int], delta: Tuple[int, int]) -> int:
    return 1000*(pos[0] + 1) + 4 * (pos[1] + 1) + DELTAS.index(delta)


def get_cube_face(pos: Tuple[int, int]) -> int:
    '''
    Input layout:
      0 1
      2
    3 4
    5
    '''
    if pos[0] <= cube_size - 1:
        return 1 if pos[1] >= cube_size * 2 else 0
    elif pos[0] <= cube_size * 2 - 1:
        return 2
    elif pos[0] <= cube_size * 3 - 1:
        return 3 if pos[1] <= cube_size - 1 else 4
    else:
        return 5


def move_cube(pos: Tuple[int, int], delta: Tuple[int, int]) -> Tuple[Tuple[int, int], Tuple[int, int]]:
    # returns (new_pos, new_delta)
    (n_x, n_y) = move(pos, delta)  # default move

    (c_x, c_y) = pos
    new_delta: Tuple[int, int] = delta
    face_idx: int = get_cube_face(pos)
    # 0 ->
    if face_idx == 0 and c_x == 0 and delta == (-1, 0):
        # 0 -> 5 (upwards)
        n_x, n_y = (3 * cube_size) + (c_y % cube_size), 0
        new_delta = (0, 1)
    if face_idx == 0 and c_y == cube_size and delta == (0, -1):
        # 0 -> 3 (leftwards)
        n_x, n_y = (3 * cube_size) - (c_x % cube_size) - 1, 0
        new_delta = (0, 1)
    # 1 ->
    if face_idx == 1 and c_x == 0 and delta == (-1, 0):
        # 1 -> 5 (upwards)
        n_x, n_y = cube_size * 4 - 1, c_y % cube_size
    if face_idx == 1 and c_y == 3 * cube_size - 1 and delta == (0, 1):
        # 1 -> 4 (rightwards)
        n_x, n_y = (2 * cube_size) + (c_x % cube_size), 2 * cube_size - 1
        new_delta = (0, -1)
    if face_idx == 1 and c_x == cube_size * 2 - 1 and delta == (1, 0):
        # 1 -> 2 (downwards)
        n_x, n_y = cube_size + (c_y % cube_size), 2 * cube_size - 1
        new_delta = (0, -1)
    # 2 ->
    if face_idx == 2 and c_y == cube_size and delta == (0, -1):
        # 2 -> 3 (leftwards)
        n_x, n_y = 2 * cube_size, c_x % cube_size
        new_delta = (1, 0)
    if face_idx == 2 and c_y == cube_size * 2 - 1 and delta == (0, 1):
        # 2 -> 1 (rightwards)
        n_x, n_y = cube_size - 1, 2 * cube_size + (c_x % cube_size)
        new_delta = (-1, 0)
    # 3 ->
    if face_idx == 3 and c_y == 0 and delta == (0, -1):
        # 3 -> 0 (leftwards)
        n_x, n_y = cube_size - 1 - (c_x % cube_size), cube_size
        new_delta = (0, 1)
    if face_idx == 3 and c_x == 2*cube_size and delta == (-1, 0):
        # 3 -> 2 (upwards)
        n_x, n_y = cube_size + c_y, cube_size
        new_delta = (0, 1)
    # 4 ->
    if face_idx == 4 and c_x == 3 * cube_size - 1 and delta == (1, 0):
        # 4 -> 5 (downwards)
        n_x, n_y = 3 * cube_size + (c_y % cube_size), cube_size - 1
        new_delta = (0, -1)
    if face_idx == 4 and c_y == 2 * cube_size - 1 and delta == (0, 1):
        # 4 -> 1 (rightwards)
        n_x, n_y = cube_size - 1 - (c_x % cube_size), 3 * cube_size - 1
        new_delta = (0, -1)
    # 5 ->
    if face_idx == 5 and c_y == 0 and delta == (0, -1):
        # 5 -> 0 (leftwards)
        n_x, n_y = 0, cube_size + (c_x % cube_size)
        new_delta = (1, 0)
    if face_idx == 5 and c_y == cube_size - 1 and delta == (0, 1):
        # 5 -> 4 (rightwards)
        n_x, n_y = 3 * cube_size - 1, cube_size + (c_x % cube_size)
        new_delta = (-1, 0)
    if face_idx == 5 and c_x == cube_size * 4 - 1 and delta == (1, 0):
        # 5 -> 1 (downwards)
        n_x, n_y = 0, (c_y % cube_size) + (2 * cube_size)

    return ((n_x, n_y), new_delta)


def walk_p1(dir_str: str, curr_pos: Tuple[int, int], curr_delta: Tuple[int, int]) -> int:
    for move_num_str, turn in re.findall("(\\d+)([L|R])?", dir_str):
        for _ in range(int(move_num_str)):
            new_pos: Tuple[int, int] = move(curr_pos, curr_delta)
            if grid[new_pos[0]][new_pos[1]] == '#':
                break

            while grid[new_pos[0]][new_pos[1]] == BLANK_CELL:
                new_pos = move(new_pos, curr_delta)
            if grid[new_pos[0]][new_pos[1]] == '#':
                break

            curr_pos = new_pos

        if turn == 'L':
            # (0, 1) -> (-1, 0) -> (0, -1) -> (1, 0) -> (0, 1)
            curr_delta = (-curr_delta[1], curr_delta[0])
        elif turn == 'R':
            # (0, 1) -> (1, 0) -> (0, -1) -> (-1, 0) -> (0, 1)
            curr_delta = (curr_delta[1], -curr_delta[0])

    return get_password(curr_pos, curr_delta)


def walk_p2(dir_str: str, curr_pos: Tuple[int, int], curr_delta: Tuple[int, int]) -> int:
    for move_num_str, turn in re.findall("(\\d+)([L|R])?", dir_str):
        for _ in range(int(move_num_str)):
            (new_pos, new_delta) = move_cube(curr_pos, curr_delta)
            if grid[new_pos[0]][new_pos[1]] == '#':
                break
            curr_pos = new_pos
            curr_delta = new_delta

        if turn == 'L':
            # (0, 1) -> (-1, 0) -> (0, -1) -> (1, 0) -> (0, 1)
            curr_delta = (-curr_delta[1], curr_delta[0])
        elif turn == 'R':
            # (0, 1) -> (1, 0) -> (0, -1) -> (-1, 0) -> (0, 1)
            curr_delta = (curr_delta[1], -curr_delta[0])

    return get_password(curr_pos, curr_delta)


curr_delta: Tuple[int, int] = DELTAS[0]
curr_pos: Tuple[int, int] = (0, grid[0].index('.'))

print(f"Part 1: {walk_p1(dir_str,curr_pos, curr_delta)}")
print(f"Part 2: {walk_p2(dir_str, curr_pos, curr_delta)}")
