from sys import argv
from math import min


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer

@value
struct MapRange(CollectionElement):
    var dst_start: Int
    var src_start: Int
    var range: Int

    fn in_src_range(self, src: Int) -> Bool:
        return src >= self.src_start and src < self.src_start + self.range

    fn in_dst_range(self, dst: Int) -> Bool:
        return dst >= self.dst_start and dst < self.dst_start + self.range

    fn get_dst(self, src: Int) -> Int:
        return self.dst_start + (src - self.src_start)

    fn get_src(self, dst: Int) -> Int:
        return self.src_start + (dst - self.dst_start)


struct Map:
    var ranges: DynamicVector[MapRange]

    fn __init__(inout self):
        self.ranges = DynamicVector[MapRange]()

    fn __copyinit__(inout self, existing: Self):
        self.ranges = existing.ranges

    fn add_range(inout self, map_range: MapRange):
        self.ranges.push_back(map_range)

    fn advance(self, val: Int) -> Int:
        let cat_map_opt = find_category_map(self.ranges, val, True)
        if cat_map_opt.get[0, Bool]():
            return self.ranges[cat_map_opt.get[1, Int]()].get_dst(val)
        return val

    fn advance_inv(self, val: Int) -> Int:
        let cat_map_opt = find_category_map(self.ranges, val, False)
        if cat_map_opt.get[0, Bool]():
            return self.ranges[cat_map_opt.get[1, Int]()].get_src(val)
        return val


fn init_category_map(map_section_str: String) raises -> Map:
    var m = Map()
    let split: DynamicVector[String] = map_section_str.split("\n")
    for idx in range(1, len(split)):
        if split[idx] == "":
            continue

        let map_vals = split[idx].split(" ")
        m.add_range(MapRange(atol(map_vals[0]), atol(map_vals[1]), atol(map_vals[2])))
    return m


fn find_category_map(
    maps: DynamicVector[MapRange], val: Int, is_src: Bool
) -> Tuple[Bool, Int]:
    for i in range(len(maps)):
        if (is_src and maps[i].in_src_range(val)) or (
            not is_src and maps[i].in_dst_range(val)
        ):
            return (True, i)
    return (False, -1)


fn main() raises:
    let almanac_str: String = read(argv()[1])

    let sections = almanac_str.split("\n\n")
    let seeds_str: DynamicVector[String] = sections[0][7:].split(" ")

    var seeds: DynamicVector[Int] = DynamicVector[Int]()
    for i in range(len(seeds_str)):
        seeds.push_back(atol(seeds_str[i]))

    let m0 = init_category_map(sections[1])
    let m1 = init_category_map(sections[2])
    let m2 = init_category_map(sections[3])
    let m3 = init_category_map(sections[4])
    let m4 = init_category_map(sections[5])
    let m5 = init_category_map(sections[6])
    let m6 = init_category_map(sections[7])

    var nums_p1 = seeds
    for i in range(len(nums_p1)):
        nums_p1[i] = m6.advance(
            m5.advance(
                m4.advance(m3.advance(m2.advance(m1.advance(m0.advance(nums_p1[i])))))
            )
        )

    var min_location_p1: Int = 10**11
    for i in range(len(nums_p1)):
        min_location_p1 = min(min_location_p1, nums_p1[i])

    print("Part 1:", min_location_p1)

    var found: Bool = False
    var min_location_p2: Int = 0
    while not found:
        let seed = m0.advance_inv(
            m1.advance_inv(
                m2.advance_inv(
                    m3.advance_inv(
                        m4.advance_inv(m5.advance_inv(m6.advance_inv(min_location_p2)))
                    )
                )
            )
        )

        for i in range(0, len(seeds), 2):
            if (seed >= seeds[i]) and (seed < seeds[i] + seeds[i + 1]):
                found = True
                break

        if not found:
            min_location_p2 += 1

    print("Part 2:", min_location_p2)
