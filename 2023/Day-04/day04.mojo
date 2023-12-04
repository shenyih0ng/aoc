from sys import argv
from python import Python
from math import min, max

from memory import memset
from memory.unsafe import DTypePointer
from utils.vector import DynamicVector


fn read(file_name: String) raises -> PythonObject:
    var file = open(file_name, "r")
    let buffer: PythonObject = file.read()
    file.close()
    return buffer.strip()


fn main() raises:
    let input_py_str = read(argv()[1])
    var cards_py_str = input_py_str.split("\n")

    var cards_num_matches: DynamicVector[Int] = DynamicVector[Int]()
    var total_points: Int = 0

    for card_py_str in cards_py_str:
        var card_points: Int = 0
        var card_matches: Int = 0

        let numbers_py_str = card_py_str.split(":")[1]
        let win_and_deck = numbers_py_str.split("|")

        var win_nums = win_and_deck[0].split(" ")
        var deck = win_and_deck[1].split(" ")

        var deck_idx: Int = 0
        for num in deck:
            if num == "":
                deck_idx += 1
                continue

            var is_win_num: Bool = False
            for win_num in win_nums:
                if win_num == num:
                    is_win_num = True
                    break

            if not is_win_num:
                deck_idx += 1
                continue

            card_points += min(deck_idx, 1) * max(card_points, 1)
            card_matches += 1
            deck_idx += 1

        total_points += card_points
        cards_num_matches.push_back(card_matches)

    print("Part 1:", total_points)

    let num_card_ids: Int = cards_num_matches.size
    let num_cards_vec: DTypePointer[DType.uint64] = DTypePointer[DType.uint64].alloc(
        num_card_ids
    )
    for _i in range(num_card_ids):
        num_cards_vec.store(_i, 1)

    var card_id: Int = 1
    while card_id <= cards_num_matches.size:
        let card_idx = card_id - 1
        let num_card_id = num_cards_vec.load(card_idx)
        for i in range(1, cards_num_matches[card_idx] + 1):
            num_cards_vec.store(
                card_idx + i, num_cards_vec.load(card_idx + i) + num_card_id
            )
        card_id += 1

    var total_num_cards: Int = 0
    for _i in range(cards_num_matches.size):
        total_num_cards += num_cards_vec.load(_i).to_int()

    print("Part 2:", total_num_cards)
