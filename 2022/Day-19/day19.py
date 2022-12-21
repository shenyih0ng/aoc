import re
import sys
import math
from typing import List, Tuple, Dict

Blueprint = List[List[Tuple[int, int]]]

ORDER = ['ore', 'clay', 'obsidian', 'geodes']
GEODES_IDX: int = ORDER.index('geodes')
INIT_BOTS: List[int] = [1, 0, 0, 0]
INIT_MATERIALS: List[int] = [0, 0, 0, 0]
MAX_TIME_P1 = 24
MAX_TIME_P2 = 32


def find_max_geodes(bp: Blueprint,
                    max_m_required: List[int],
                    bots: List[int],
                    materials: List[int],
                    time_left: int,
                    cache: Dict[Tuple[int, ...], int]) -> int:
    if time_left == 0:
        return materials[GEODES_IDX]
    elif time_left < 0:
        # exceed time limit
        return 0

    key: Tuple[int, ...] = (*bots, *materials, time_left)
    if key in cache:
        return cache[key]

    curr_max_geodes: int = bots[3] * time_left + materials[GEODES_IDX]
    for b_idx, bot in enumerate(bp):
        if bots[b_idx] >= max_m_required[b_idx] and b_idx != GEODES_IDX:
            # we dont need more bots than materials needed
            # the time taken to produce will be same since one action
            continue

        can_build: bool = True
        delay: int = 0
        for m_amt, m_type_idx in bot:
            if bots[m_type_idx] == 0:
                can_build = False
                break
            delay = max(delay, math.ceil(
                (m_amt - materials[m_type_idx]) / bots[m_type_idx]))

        if not can_build:
            continue

        delay += 1  # for building
        time_remaining: int = time_left - delay

        new_materals: List[int] = [b_num * delay +
                                   m_num for b_num, m_num in zip(bots, materials)]
        for m_amt, m_type_idx in bot:
            new_materals[m_type_idx] -= m_amt

        new_bots: List[int] = bots.copy()
        new_bots[b_idx] += 1

        curr_max_geodes = max(curr_max_geodes, find_max_geodes(
            bp, max_m_required, new_bots, new_materals, time_remaining, cache))

    cache[key] = curr_max_geodes
    return curr_max_geodes


input: list[str] = sys.stdin.read().strip().split('\n')

bp_idx: int = 1
total_quality_level: int = 0
max_geodes_product: int = 1
for bp_str in input:
    bp: Blueprint = []
    max_m_required: List[int] = [0, 0, 0, 0]
    for bot in bp_str[bp_str.index(':') + 1::].split('. '):
        materials: List[Tuple[int, int]] = []
        for m_amt_str, m_type_str in re.findall("(\\d+) (\\w+)", bot):
            m_amt: int = int(m_amt_str)
            m_type_idx: int = ORDER.index(m_type_str)
            max_m_required[m_type_idx] = max(max_m_required[m_type_idx], m_amt)
            materials.append((m_amt, m_type_idx))
        bp.append(materials)

    bp_cache = dict()  # shared cache
    bp_max_geodes_p1: int = find_max_geodes(bp, max_m_required, INIT_BOTS.copy(),
                                            INIT_MATERIALS.copy(), MAX_TIME_P1, bp_cache)
    total_quality_level += bp_max_geodes_p1 * bp_idx

    if bp_idx <= 3:
        # surprisingly input blueprints have a bearable exec time
        # test blueprints took forever
        bp_max_geodes_p2: int = find_max_geodes(bp, max_m_required, INIT_BOTS.copy(),
                                                INIT_MATERIALS.copy(), MAX_TIME_P2, bp_cache)
        max_geodes_product *= bp_max_geodes_p2
    bp_idx += 1


print(f"Part 1: {total_quality_level}")
print(f"Part 2: {max_geodes_product}")
