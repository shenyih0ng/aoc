import sys
from functools import cmp_to_key, reduce

DIVIDER_PACKETS = [[[2]], [[6]]]


def is_right_order(p1, p2) -> int:
    # comparator function
    if (isinstance(p1, int) and isinstance(p2, int)):
        return -1 if p1 < p2 else 1
    elif (isinstance(p1, list) and isinstance(p2, list)):
        for i in range(len(p1)):
            if i >= len(p2):
                return 1  # p2 runs out first
            p1_ele, p2_ele = (p1[i], p2[i])
            if is_right_order(p1_ele, p2_ele) == -1:  # p1_ele < p2_ele
                return -1
            if is_right_order(p2_ele, p1_ele) == -1:  # p2_ele < p1_ele
                return 1
        # False -> both are same length && cannot determine thru comparsion
        # also implies that is_right_order(p1, p2) == is_right_order(p2, p1) == False
        return -1 if len(p1) < len(p2) else 1
    else:
        p1 = [p1] if isinstance(p1, int) else p1
        p2 = [p2] if isinstance(p2, int) else p2
        return is_right_order(p1, p2)


input: str = sys.stdin.read().strip()
processed_p1 = map(lambda x: map(lambda y: eval(y), x.split("\n")), input.split("\n\n"))

idx_sum = 0  # sum of right order indices
for idx, p in enumerate(processed_p1):
    p1, p2 = p
    if (is_right_order(p1, p2) < 0):
        idx_sum += idx + 1

print(f"Part 1: {idx_sum}")

processed_p2 = list(map(lambda x: eval(x), input.replace(
    "\n\n", "\n").split("\n"))) + DIVIDER_PACKETS
sorted_p2 = sorted(processed_p2, key=cmp_to_key(is_right_order))

print(f"Part 2: {reduce(lambda x, y: x * y, (map(lambda x : sorted_p2.index(x) + 1, DIVIDER_PACKETS)))}")
