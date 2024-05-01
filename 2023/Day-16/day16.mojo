# mojo (nightly) 2024.4.2923 (f54d89e3)
from sys import argv
from math import max
from collections import List, Set


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    var buffer: String = file.read()[:-1]
    file.close()
    return buffer


fn _max(*args: Int) -> Int:
    var max_val = args[0]
    for val in args:
        max_val = max(max_val, val)
    return max_val


@value
struct Vec2D(KeyElement):
    var x: Int
    var y: Int

    fn __hash__(self) -> Int:
        return (53 + hash(self.x)) * 53 + hash(self.y)

    fn __eq__(self, other: Vec2D) -> Bool:
        return self.x == other.x and self.y == other.y

    fn __ne__(self, other: Vec2D) -> Bool:
        return not (self == other)


@value
struct Beam(CollectionElement, KeyElement):
    var pos: Vec2D
    var delta: Vec2D

    fn step(inout self) -> None:
        self.pos.x += self.delta.x
        self.pos.y += self.delta.y

    fn __hash__(self) -> Int:
        return hash(self.pos) * hash(self.delta)

    fn __eq__(self, other: Beam) -> Bool:
        return self.pos == other.pos and self.delta == other.delta

    fn __ne__(self, other: Beam) -> Bool:
        return not (self == other)


fn get_num_energized(
    start_beam: Beam, n_rows: Int, n_cols: Int, map: List[String]
) -> Int:
    var energized = Set[Vec2D]()

    var active_beams = List[Beam](start_beam)
    var uq_beams = Set[Beam](start_beam)

    fn add_beam(
        new_beam: Beam,
        inout uq_beams: Set[Beam],
        inout active_beams: List[Beam],
    ) -> None:
        if new_beam not in uq_beams:
            uq_beams.add(new_beam)
            active_beams.append(new_beam)

    while len(active_beams):
        var beam = active_beams.pop(0)
        beam.step()

        if (
            beam.pos.x < 0
            or beam.pos.y < 0
            or beam.pos.x >= n_cols
            or beam.pos.y >= n_rows
        ):
            # out of range
            continue

        energized.add(beam.pos)

        var map_element = map[beam.pos.y][beam.pos.x]
        if map_element == ".":
            active_beams.append(beam)
        elif map_element == "|":
            if beam.delta.y != 0:
                active_beams.append(beam)
            add_beam(Beam(beam.pos, Vec2D(0, 1)), uq_beams, active_beams)
            add_beam(Beam(beam.pos, Vec2D(0, -1)), uq_beams, active_beams)
        elif map_element == "-":
            if beam.delta.x != 0:
                active_beams.append(beam)
            add_beam(Beam(beam.pos, Vec2D(1, 0)), uq_beams, active_beams)
            add_beam(Beam(beam.pos, Vec2D(-1, 0)), uq_beams, active_beams)
        elif map_element == "/":
            add_beam(
                Beam(beam.pos, Vec2D(-beam.delta.y, -beam.delta.x)),
                uq_beams,
                active_beams,
            )
        elif map_element == "\\":
            add_beam(
                Beam(beam.pos, Vec2D(beam.delta.y, beam.delta.x)),
                uq_beams,
                active_beams,
            )

    return len(energized)


fn main() raises:
    var map = read(argv()[1]).split("\n")
    var n_rows = len(map)
    var n_cols = len(map[0])

    fn _get_num_energized(start_beam: Beam) -> Int:
        return get_num_energized(start_beam, n_rows, n_cols, map)

    print("Part 1:", _get_num_energized(Beam(Vec2D(-1, 0), Vec2D(1, 0))))

    var max_energized = 0
    # left-right
    for y in range(n_rows):
        max_energized = _max(
            max_energized,
            _get_num_energized(Beam(Vec2D(-1, y), Vec2D(1, 0))),
            _get_num_energized(Beam(Vec2D(n_cols, y), Vec2D(-1, 0))),
        )
    # top-bottom
    for x in range(n_rows):
        max_energized = _max(
            max_energized,
            _get_num_energized(Beam(Vec2D(x, -1), Vec2D(0, 1))),
            _get_num_energized(Beam(Vec2D(x, n_rows), Vec2D(0, -1))),
        )
    print("Part 2:", max_energized)
