import sys
from typing import List, Tuple, Dict, Set

# [...columns, rock_idx, jet_idx]
State = Tuple[int, ...]

WIDTH = 7
ROCKS = [
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)],
    [(0, 2), (1, 2), (2, 0), (2, 1), (2, 2)],
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(0, 0), (0, 1), (1, 0), (1, 1)]
]
ROCK_HEIGHTS = list(map(lambda rock: max(rock, key=lambda x: x[0])[
    0] - min(rock, key=lambda x: x[0])[0] + 1, ROCKS))

jet_pattern: List[int] = list(
    map(lambda x: 1 if x == '>' else -1, sys.stdin.read().strip()))


def move(rock: List[Tuple[int, int]], delta: Tuple[int, int]) -> List[Tuple[int, int]]:
    return list(map(lambda r: (r[0] + delta[0], r[1] + delta[1]), rock))


def is_valid_pos(rock: List[Tuple[int, int]], peaks) -> bool:
    for coord in rock:
        if coord[1] >= WIDTH or coord[1] < 0 or coord[0] in peaks[coord[1]]:
            return False
    return True


def get_max_peak(peaks: Dict[int, Set[int]]) -> int:
    return max(map(lambda peak: max(peak), peaks.values()))


def get_state(peaks: Dict[int, Set[int]], rock_idx: int, jet_idx) -> State:
    max_peaks: List[int] = list(map(lambda peak: max(peak), peaks.values()))
    max_peaks_norm: List[int] = [max_peak -
                                 max(max_peaks) for max_peak in max_peaks]
    return tuple(max_peaks_norm + [rock_idx, jet_idx])


def simulate(num_rocks: int) -> int:
    peaks: Dict[int, Set[int]] = dict([(i, {0}) for i in range(7)])
    # State : (height, num_rocks)
    states: Dict[State, Tuple[int, int]] = dict()
    height_offset: int = 0

    count = 0
    rock_idx = 0
    jet_idx = 0
    while count < num_rocks:
        done: bool = False
        rock: List[Tuple[int, int]] = list(
            map(lambda coord: (get_max_peak(peaks) + 3 + ROCK_HEIGHTS[rock_idx] - coord[0], coord[1] + 2), ROCKS[rock_idx]))
        while not done:
            curr_state: State = get_state(peaks, rock_idx, jet_idx)
            if curr_state not in states:
                states[curr_state] = (get_max_peak(peaks), count)
            else:
                found_state: Tuple[int, int] = states[curr_state]
                height_per_cycle: int = get_max_peak(peaks) - found_state[0]
                rocks_per_cycle: int = count - found_state[1]

                num_cycles_left: int = (num_rocks - count) // rocks_per_cycle
                height_offset = num_cycles_left * height_per_cycle
                count = num_rocks - \
                    ((num_rocks - found_state[1]) % rocks_per_cycle)
                # reset so that it doesn't use invalidated states since there is > 1 cycle
                states = {}

            rock_jet: List[Tuple[int, int]] = move(
                rock, (0, jet_pattern[jet_idx]))
            if not is_valid_pos(rock_jet, peaks):
                rock_jet = rock

            rock_drop: List[Tuple[int, int]] = move(rock_jet, (-1, 0))
            if not is_valid_pos(rock_drop, peaks):
                done = True
                rock = rock_jet
            else:
                rock = rock_drop
            jet_idx = (jet_idx + 1) % len(jet_pattern)

        for coord in rock:
            peaks[coord[1]].add(coord[0])

        rock_idx = (rock_idx + 1) % len(ROCKS)
        count += 1

    return get_max_peak(peaks) + height_offset


print(f"Part 1: {simulate(2022)}")
print(f"Part 2: {simulate(1000000000000)}")
