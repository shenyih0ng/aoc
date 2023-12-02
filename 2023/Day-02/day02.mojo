from sys import argv

alias NUM_RED: Int = 12
alias NUM_GREEN: Int = 13
alias NUM_BLUE: Int = 14


fn read(file_name: String) raises -> String:
    var file = open(file_name, "r")
    let buffer = file.read()
    file.close()
    return buffer


fn is_possible(game: String) raises -> Bool:
    fn is_possible_set(game_set: String) raises -> Bool:
        var i: Int = 0
        while i < len(game_set):
            var j: Int = 0
            var whitespace_idx: Int = -1
            while game_set[i + j] != "," and i + j < len(game_set):
                if whitespace_idx == -1 and game_set[i + j] == " ":
                    whitespace_idx = i + j
                j += 1

            debug_assert(whitespace_idx != -1, "should not be -1")

            let num_cubes: Int = atol(game_set[i:whitespace_idx])
            let cube_color: String = game_set[whitespace_idx + 1 : i + j]

            if (cube_color == "red" and num_cubes > NUM_RED) or
                (cube_color == "green" and num_cubes > NUM_GREEN) or
                    (cube_color == "blue" and num_cubes > NUM_BLUE):
                return False

            i += j + 2  # account for extra whitespace delimiter

        return True

    var i: Int = 0
    while i < len(game):
        var j: Int = 0
        while game[i + j] != ";" and i + j < len(game):
            j += 1

        let game_set = game[i : i + j]

        if not is_possible_set(game_set):
            return False

        i += j + 2  # account for extra whitespace delimiter

    return True


fn get_power(game: String) raises -> Int:
    var max_red: Int = -1
    var max_green: Int = -1
    var max_blue: Int = -1

    var i: Int = 0
    while i < len(game):
        var j: Int = 0
        var whitespace_idx: Int = -1
        while game[i + j] != "," and game[i + j] != ";" and i + j < len(game):
            if whitespace_idx == -1 and game[i + j] == " ":
                whitespace_idx = i + j
            j += 1

        debug_assert(whitespace_idx != -1, "should not be -1")

        let num_cubes: Int = atol(game[i:whitespace_idx])
        let cube_color: String = game[whitespace_idx + 1 : i + j]

        if cube_color == "red" and num_cubes > max_red:
            max_red = num_cubes
        if cube_color == "green" and num_cubes > max_green:
            max_green = num_cubes
        if cube_color == "blue" and num_cubes > max_blue:
            max_blue = num_cubes

        i += j + 2  # account for extra whitespace delimiter

    return max_red * max_green * max_blue


fn main() raises:
    let games: String = read(argv()[1])

    var possible_game_id_sum: Int = 0
    var power_sum: Int = 0

    var i: Int = 0
    while i < len(games):
        var j: Int = 0
        while games[i + j] != "\n":
            j += 1

        let game: String = games[i : i + j]
        let game_id: Int = atol(
            game[5] if game[6] == ":" else (game[5:7] if game[7] == ":" else game[5:8])
        )

        let game_sets: String = game[8 if game_id < 10 else (9 if game_id < 100 else 10) :]

        if is_possible(game_sets):
            possible_game_id_sum += game_id
        power_sum += get_power(game_sets)

        i += j + 1

    print("Part 1:", possible_game_id_sum)
    print("Part 2:", power_sum)
