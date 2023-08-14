import sys
from typing import Union

moves: list[str] = sys.stdin.read().strip().split(', ')

x: int = 0
y: int = 0
bearing: int = 0 # starting angle (+ clockwise, - anti-clockwise)

visited: dict[tuple[int, int]] = {}
bunny_hq_loc: Union[tuple[int, int], None] = None

for m in moves:
	direction: str = m[0]
	amount: int = int(m[1::])
	
	bearing += 90 if direction == "R" else (-90)
	norm_bearing: int = int(bearing % 360)

	move_x: bool = norm_bearing in [90, 270]
	move_y: bool = norm_bearing in [0, 180]
	assert not (move_x and move_y)

	x_scale: int = 1 if norm_bearing == 90 else -1
	y_scale: int = 1 if norm_bearing == 0 else -1

	x_delta: int = x_scale * amount * move_x
	y_delta: int = y_scale * amount * move_y
	delta: tuple[int, int] = (move_x * x_scale, move_y * y_scale)

	for _ in range(0, abs(x_delta or y_delta)):
		x += delta[0]
		y += delta[1]
		if (x, y) in visited and bunny_hq_loc is None:
			bunny_hq_loc = (x, y)	
		visited[(x, y)] = 1
	
print(f"Part 1: {abs(x) + abs(y)}")
print(f"Part 2: {abs(bunny_hq_loc[0]) + abs(bunny_hq_loc[1])}")
	