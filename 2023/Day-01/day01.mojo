from sys import argv


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer = file.read()
    file.close()
    return buffer


fn get_digit(c: String, idx: Int, include_letters: Bool) raises -> Int:
    if ord(c[idx]) >= ord("0") and ord(c[idx]) <= ord("9"):
        return atol(c[idx])

    if not include_letters:
        return -1

    let one_two_six = c[idx : idx + 3]
    if one_two_six == "one":
        return 1
    if one_two_six == "two":
        return 2
    if one_two_six == "six":
        return 6

    let four_five_nine = c[idx : idx + 4]
    if four_five_nine == "four":
        return 4
    if four_five_nine == "five":
        return 5
    if four_five_nine == "nine":
        return 9

    let three_seven_eight = c[idx : idx + 5]
    if three_seven_eight == "three":
        return 3
    if three_seven_eight == "seven":
        return 7
    if three_seven_eight == "eight":
        return 8

    return -1


fn get_calibration_val(text: String, include_letters: Bool) raises -> Int:
    var first_digit: Int = -1
    var last_digit: Int = -1
    for i in range(len(text)):
        let maybe_digit: Int = get_digit(text, i, include_letters)
        if maybe_digit == -1:
            continue

        if first_digit == -1:
            first_digit = maybe_digit
        else:
            last_digit = maybe_digit

    if last_digit == -1:
        last_digit = first_digit

    return first_digit * 10 + last_digit


fn main() raises:
    let document: String = read(argv()[1])

    var total_calib_val_p1: Int = 0
    var total_calib_val_p2: Int = 0

    var i: Int = 0
    while i < len(document):
        var j: Int = 0
        while document[i + j] != "\n":
            j += 1

        let text: String = document[i : i + j]
        total_calib_val_p1 += get_calibration_val(text, False)
        total_calib_val_p2 += get_calibration_val(text, True)
        i += j + 1

    print("Part 1:", total_calib_val_p1)
    print("Part 2:", total_calib_val_p2)
