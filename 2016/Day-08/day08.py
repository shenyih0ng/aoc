import sys

data = sys.stdin.read().strip().split('\n')

screen = [ [0] * 50 for _ in range(6) ]

rotate_row_buffer = [0] * 50
rotate_col_buffer = [0] * 6

def do (inst: str) -> None:
    tokens = inst.split(" ")

    if tokens[0] == "rect":
        width, height = list(map(int, tokens[1].split("x")))
        for w in range(0, width):
            for h in range(0, height):
                screen[h][w] = 1

    elif tokens[0] == "rotate":
        start, delta = int(tokens[2].split("=")[-1]), int(tokens[4])
        if tokens[1] == "row":
            for i, v in enumerate(screen[start]):
                rotate_row_buffer[(i + delta) % 50] = v
            for i, v in enumerate(rotate_row_buffer):
                screen[start][i] = v
        else:
            for j in range(6):
                rotate_col_buffer[((j + delta) % 6)] = screen[j][start]
            for j, v in enumerate(rotate_col_buffer):
                screen[j][start] = v

for inst in data:
    do(inst)

print("Part 1:", sum([sum(row) for row in screen]))

for row in screen:
    print("".join(['#' if x else ' ' for x in row]))

"""
 ##  #### ###  #  # ###  #### ###    ## ###   ###
#  # #    #  # #  # #  #    # #  #    # #  # #
#  # ###  ###  #  # #  #   #  ###     # #  # #
#### #    #  # #  # ###   #   #  #    # ###   ##
#  # #    #  # #  # #    #    #  # #  # #       #
#  # #    ###   ##  #    #### ###   ##  #    ###
"""

