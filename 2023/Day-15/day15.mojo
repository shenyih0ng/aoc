from sys import argv


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()[:-1]
    file.close()
    return buffer


fn hash(str: String) -> Int:
    var hash: Int = 0
    for i in range(len(str)):
        hash += ord(str[i])
        hash *= 17
        hash %= 256
    return hash


@value
struct Lens(CollectionElement):
    var label: String
    var focal_length: Int


@value
struct Box(CollectionElement):
    var slots: DynamicVector[Lens]
    var next_free: Int

    fn __init__(inout self):
        self.slots = DynamicVector[Lens]()
        self.slots.resize(9, Lens(-1, -1))
        self.next_free = 0

    fn add(inout self, lens: Lens):
        for i in range(0, self.next_free):
            if self.slots[i].label == lens.label:
                self.slots[i].focal_length = lens.focal_length
                return
        self.slots[self.next_free] = lens
        self.next_free += 1

    fn remove(inout self, label: String):
        var label_idx = -1
        for i in range(len(self.slots)):
            if self.slots[i].label == label:
                label_idx = i
                break
        if label_idx != -1:
            for j in range(label_idx + 1, len(self.slots)):
                self.slots[j - 1] = self.slots[j]
            self.next_free -= 1


fn main() raises:
    let input = read(argv()[1])
    let steps = input.split(",")

    var boxes = DynamicVector[Box]()
    for _i in range(256):
        boxes.push_back(Box())

    var sum: Int = 0
    for i in range(len(steps)):
        let step = steps[i]
        sum += hash(step)

        var op_idx: Int = 0
        while step[op_idx] != "=" and step[op_idx] != "-":
            op_idx += 1

        let label = step[:op_idx]
        let box_idx = hash(label)
        if step[op_idx] == "-":
            boxes[box_idx].remove(label)
        else:
            let focal_length = atol(step[op_idx + 1 :])
            boxes[box_idx].add(Lens(label, focal_length))

    print("Part 1:", sum)

    var total_focusing_power: Int = 0
    for i in range(len(boxes)):
        let box = boxes[i]
        for j in range(0, box.next_free):
            let lens = box.slots[j]
            total_focusing_power += (i + 1) * (j + 1) * lens.focal_length

    print("Part 2:", total_focusing_power)
