from utils.vector import DynamicVector


fn read(fname: String) raises -> String:
    var f: FileHandle = open(fname, "r")
    let data: String = f.read()[:-1]  # remove trailing whitespace
    f.close()
    return data


struct Room:
    var name: String
    var sector_id: Int
    var checksum: String

    fn __init__(inout self, room_str: String) raises:
        var i: Int = 0

        var name_end: Int = -1
        var sector_end: Int = -1

        while i < len(room_str):
            let c_int: Int = ord(room_str[i])
            if name_end == -1 and c_int >= ord("0") and c_int <= ord("9"):
                name_end = i - 1
            if sector_end == -1 and room_str[i] == "[":
                sector_end = i
            i += 1

        self.name = room_str[:name_end]
        self.sector_id = atol(room_str[name_end + 1 : sector_end])
        self.checksum = room_str[sector_end + 1 : -1]

    fn is_real(self) -> Bool:
        var char_counts: DynamicVector[Int] = DynamicVector[Int](26)
        char_counts.resize(26)

        var i: Int = 0
        while i < len(self.name):
            if self.name[i] != "-":
                let c_int: Int = ord(self.name[i])
                char_counts[c_int - ord("a")] += 1
            i += 1

        var j: Int = 0
        var checksum: String = ""
        while j < len(self.checksum):
            var max_count: Int = 0
            var max_index: Int = -1

            var k: Int = 0
            while k < len(char_counts):
                if char_counts[k] > max_count:
                    max_count = char_counts[k]
                    max_index = k
                k += 1

            checksum += chr(max_index + ord("a"))
            char_counts[max_index] = 0
            j += 1

        return checksum == self.checksum

    fn decrypt(self) -> String:
        var decrypted: String = ""

        var i: Int = 0
        while i < len(self.name):
            if self.name[i] == "-":
                decrypted += " "
            else:
                let c_int: Int = ord(self.name[i])
                let new_c_int: Int = (c_int - ord("a") + self.sector_id) % 26 + ord("a")
                decrypted += chr(new_c_int)

            i += 1

        return decrypted

    fn dump(self):
        print(self.name, self.sector_id, self.checksum)


fn main() raises:
    let data: String = read("input.txt")

    var sum_sector_real_rooms: Int = 0
    var sector_id_northpole: Int = 0

    var i: Int = 0
    var j: Int = 0
    while i < len(data):
        while j < len(data) and data[j] != "\n":
            j += 1

        let room: Room = Room(data[i:j])
        if room.is_real():
            sum_sector_real_rooms += room.sector_id

        if room.decrypt() == "northpole object storage":
            sector_id_northpole = room.sector_id

        i = j + 1
        j = i

    print("Part 1:", sum_sector_real_rooms)
    print("Part 2:", sector_id_northpole)
