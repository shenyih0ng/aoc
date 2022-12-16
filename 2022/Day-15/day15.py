import re
import sys
from typing import Tuple, Optional, List

ROW: int = 2000000
DISTRESS: int = 4000000


def manhattan_dist(p1: Tuple[int, int], p2: Tuple[int, int]) -> int:
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def overlap(r1: Tuple[int, int], r2: Tuple[int, int]) -> bool:
    return max(r1[0], r2[0]) <= min(r1[1], r2[1])


def get_overlap(r1: Tuple[int, int], r2: Tuple[int, int]) -> Tuple[int, int]:
    return (max(r1[0], r2[0]), min(r1[1], r2[1]))


def range_len(r: Tuple[int, int]) -> int:
    return r[1] - r[0] + 1


def merge_ranges(ranges: List[Tuple[int, int]], bound: Tuple[int, int]) -> List[Tuple[int, int]]:
    '''
    ranges: list of ranges
    bound: bounding constraint

    returns: merged ranges within specified bound
    '''
    ranges_within_bound: List[Tuple[int, int]] = list(map(lambda range: get_overlap(
        range, bound), filter(lambda range: overlap(range, bound), ranges)))
    ranges_within_bound.sort()

    stack = [ranges_within_bound[0]]
    for range in ranges_within_bound[1:]:
        if stack[-1][0] <= range[0] <= stack[-1][1]:
            stack[-1] = (stack[-1][0], max(stack[-1][1], range[1]))
        else:
            stack.append(range)

    return stack


def get_signals_at_row(row: int, s_coord: Tuple[int, int], radius: int) -> Optional[Tuple[int, int]]:
    '''
    s_coord: sensor coordinates
    radius: taxicab radius

    returns: signal range
    '''
    if (row > s_coord[1] + radius) or (row < s_coord[1] - radius):
        return None

    row_radius = radius - abs(s_coord[1] - row)
    return (s_coord[0] - row_radius, s_coord[0] + row_radius)


def get_non_beacon_ranges(s_coords: List[Tuple[int, int]],
                          b_coords: List[Tuple[int, int]], row: int) -> Tuple[int, List[Tuple[int, int]]]:
    '''
    returns: (num non-beacon positions, list of non beacon ranges)
    '''
    total_signal_at_row: int = 0
    num_beacon_at_row: int = len(
        set(map(lambda coord: coord[0], filter(lambda coord: coord[1] == row, b_coords))))

    signal_ranges: List[Tuple[int, int]] = []
    x_signal_ranges: set[Tuple[int, int]] = set()
    for s_coord, b_coord in zip(s_coords, b_coords):
        curr_range = get_signals_at_row(
            row, s_coord, manhattan_dist(s_coord, b_coord))
        if curr_range is None:
            continue

        for x_range in filter(lambda x_range: overlap(x_range, curr_range), x_signal_ranges):
            total_signal_at_row += range_len(
                get_overlap(x_range, curr_range))

        for s_range in filter(lambda s_range: overlap(s_range, curr_range), signal_ranges):
            r_overlap = get_overlap(s_range, curr_range)
            x_signal_ranges.add(r_overlap)
            total_signal_at_row -= range_len(r_overlap)

        total_signal_at_row += range_len(curr_range)
        signal_ranges.append(curr_range)

    return (total_signal_at_row - num_beacon_at_row, signal_ranges)


s_coords: List[Tuple[int, int]] = []
b_coords: List[Tuple[int, int]] = []

input = sys.stdin.read().strip().split('\n')
for line in input:
    matches = re.findall("x=(-?\d+), y=(-?\d+)", line)
    s_coords.append(tuple(map(int, matches[0])))
    b_coords.append(tuple(map(int, matches[1])))

print(f"Part 1: {get_non_beacon_ranges(s_coords, b_coords, ROW)[0]}")

tuning_freq: int = -1
for row in range(DISTRESS, -1, -1):  # bottom up hacks
    (_, non_beacon_ranges) = get_non_beacon_ranges(s_coords, b_coords, row)
    merged = merge_ranges(non_beacon_ranges, (0, DISTRESS))
    if len(merged) == 2:
        # we know that there is only one distress signal
        tuning_freq = (merged[0][1] + 1) * DISTRESS + row
        break

print(f"Part 2: {tuning_freq}")
