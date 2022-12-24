import sys
from collections import deque
from typing import List, Tuple

Coord = Tuple[int, int]
DELTAS: List[Coord] = [(0, 1), (0, -1), (1, 0), (-1, 0),
                       (0, 0)]  # (0,0) for wait branch

input: List[str] = sys.stdin.read().strip().split('\n')

start: Coord = (0, input[0].index('.'))
end: Coord = (len(input) - 1, input[-1].index('.'))

blizzards: List[Tuple[Coord, Coord]] = []
for r, r_str in enumerate(input):
    for c, c_str in enumerate(r_str):
        coord: Tuple[int, int] = (r, c)
        if (c_str == '>'):
            blizzards.append((coord, (0, 1)))
        elif (c_str == '<'):
            blizzards.append((coord, (0, -1)))
        elif (c_str == '^'):
            blizzards.append((coord, (-1, 0)))
        elif (c_str == 'v'):
            blizzards.append((coord, (1, 0)))


def is_out_of_bound(coord: Coord) -> bool:
    if coord == start or coord == end:
        return False
    return coord[0] <= 0 or coord[0] >= len(input) - 1 or coord[1] <= 0 or coord[1] >= len(input[0]) - 1


def blizzard_pos_at_t(blizzard: Tuple[Coord, Coord], t: int) -> Coord:
    norm_pos: Coord = (blizzard[0][0] - 1, blizzard[0][1] - 1)
    norm_pos_at_t: Coord = ((norm_pos[0] + blizzard[1][0] * t) % (
        len(input) - 2), (norm_pos[1] + blizzard[1][1] * t) % (len(input[0]) - 2))
    return (norm_pos_at_t[0] + 1, norm_pos_at_t[1] + 1)


def has_blizzard(coord: Coord, t: int) -> bool:
    blizzard_pos = map(lambda b: blizzard_pos_at_t(b, t), blizzards)
    return any(map(lambda b_coord: b_coord == coord, blizzard_pos))


def bfs(start: Coord, end: Coord, t: int = 0) -> int:
    queue = deque()
    seen = set()

    queue.append((start, t))
    seen.add((start, t))
    while len(queue) > 0:
        (curr_coord, curr_t) = queue.popleft()
        if curr_coord == end:
            return curr_t

        for adj in map(lambda d: (curr_coord[0] + d[0], curr_coord[1] + d[1]), DELTAS):
            if is_out_of_bound(adj) or (adj, curr_t + 1) in seen or has_blizzard(adj, curr_t + 1):
                continue
            queue.append((adj, curr_t + 1))
            seen.add((adj, curr_t + 1))

    return -1


t1: int = bfs(start, end)
t2: int = bfs(start, end, bfs(end, start, t1))

print(f"Part 1: {t1}")
print(f"Part 2: {t2}") # not too slow with pypy
