from sys import argv
from algorithm.sort import sort
from utils.vector import DynamicVector


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer: String = file.read()
    file.close()
    return buffer


fn counter(str: String, ignore_j: Bool) -> DynamicVector[Int]:
    var freq = DynamicVector[Int]()
    var _chars = DynamicVector[String]()

    for i in range(len(str)):
        let c = str[i]
        if ignore_j and c == "J":
            continue

        var found: Bool = False
        for j in range(len(_chars)):
            if _chars[j] == c:
                freq[j] += 1
                found = True
                break
        if not found:
            _chars.push_back(c)
            freq.push_back(1)

    return freq


@value
struct Hand(CollectionElement):
    var hand: String
    var bid: Int
    var type: Int

    fn __init__(inout self, hand: String, bid: Int, joker: Bool):
        var freq = counter(hand, joker)
        sort(freq)

        if joker:
            if len(freq) == 0:
                freq.push_back(len(hand))
            else:
                var num_jokers: Int = 0
                for i in range(len(hand)):
                    if hand[i] == "J":
                        num_jokers += 1
                freq[len(freq) - 1] += num_jokers

        if len(freq) == 1 and freq[0] == 5:
            self.type = 6
        elif len(freq) == 2 and freq[0] == 1 and freq[1] == 4:
            self.type = 5
        elif len(freq) == 2 and freq[0] == 2 and freq[1] == 3:
            self.type = 4
        elif len(freq) == 3 and freq[0] == 1 and freq[1] == 1 and freq[2] == 3:
            self.type = 3
        elif len(freq) == 3 and freq[0] == 1 and freq[1] == 2 and freq[2] == 2:
            self.type = 2
        elif (
            len(freq) == 4
            and freq[0] == 1
            and freq[1] == 1
            and freq[2] == 1
            and freq[3] == 2
        ):
            self.type = 1
        else:
            self.type = 0

        self.hand = ""
        for h_idx in range(len(hand)):
            let c = hand[h_idx]
            if c == "T":
                self.hand += chr(ord("9") + 1)
            elif c == "J":
                self.hand += chr(ord("9") + 2) if not joker else "1"  # ord("2") - 1
            elif c == "Q":
                self.hand += chr(ord("9") + 3)
            elif c == "K":
                self.hand += chr(ord("9") + 4)
            elif c == "A":
                self.hand += chr(ord("9") + 5)
            else:
                self.hand += c

        self.bid = bid

    fn __lt__(self, other: Self) -> Bool:
        if self.type < other.type:
            return True
        if self.type > other.type:
            return False

        var i: Int = 0
        while ord(self.hand[i]) == ord(other.hand[i]):
            i += 1

        return ord(self.hand[i]) < ord(other.hand[i])


fn sort_hands(inout hands: DynamicVector[Hand]) -> None:
    for i in range(len(hands)):
        var min_idx = i
        for j in range(i + 1, len(hands)):
            if hands[j] < hands[min_idx]:
                min_idx = j
        let temp = hands[i]
        hands[i] = hands[min_idx]
        hands[min_idx] = temp


fn main() raises:
    let input: String = read(argv()[1])
    let hands_str = input.split("\n")

    var hands_p1 = DynamicVector[Hand]()
    var hands_p2 = DynamicVector[Hand]()
    for h_idx in range(len(hands_str)):
        if hands_str[h_idx] == "":
            continue
        let hand_bid = hands_str[h_idx].split(" ")
        hands_p1.push_back(Hand(hand_bid[0], atol(hand_bid[1]), False))
        hands_p2.push_back(Hand(hand_bid[0], atol(hand_bid[1]), True))

    sort_hands(hands_p1)
    sort_hands(hands_p2)

    var winnings_p1: Int = 0
    var winnings_p2: Int = 0
    for rank in range(1, len(hands_p1) + 1):
        winnings_p1 += hands_p1[rank - 1].bid * rank
        winnings_p2 += hands_p2[rank - 1].bid * rank

    print("Part 1:", winnings_p1)
    print("Part 2:", winnings_p2)
