import sys
from operator import itemgetter

data = sys.stdin.read().strip().split("\n")

def get_repetition_code(freq: dict, min_max_func) -> str:
    return ''.join(map(itemgetter(1), 
                   sorted(map(lambda i : (i, min_max_func(freq[i], key=freq[i].get)), freq))))

freq = {}
for line in data:
    for i, c in enumerate(line):
        if i not in freq:
            freq[i] = {}
        if c not in freq[i]:
            freq[i][c] = 0
        freq[i][c] += 1


print(f"Part 1: {get_repetition_code(freq, max)}")
print(f"Part 2: {get_repetition_code(freq, min)}")

