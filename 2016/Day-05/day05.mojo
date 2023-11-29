from python import Python
from md5 import md5


def int_to_string(i: Int) -> String:
    return PythonObject(i).to_string()  # feels like a hack


fn pw1(input: StringRef) raises -> String:
    var pw: String = ""

    var n: Int = 8
    var num: Int = 0
    while n > 0:
        let hash: String = md5(input + int_to_string(num))
        if hash[:5] == "00000":
            pw += hash[5]
            n -= 1
        num += 1

    return pw


fn pw2(input: StringRef) raises -> String:
    var pw: String = "        "

    var n: Int = 8
    var num: Int = 0
    while n > 0:
        let hash: String = md5(input + int_to_string(num))
        if hash[:5] == "00000":
            let pos6: String = hash[5]
            if ord(pos6) >= ord("0") and ord(pos6) <= ord("7"):
                let pos6_int: Int = atol(pos6)
                if pw[pos6_int] == " ":
                    n -= 1
                    pw = pw[:pos6_int] + hash[6] + pw[pos6_int + 1 :]
        num += 1

    return pw


fn main() raises:
    let input = "ojvtpuvg"
    print("Part 1:", pw1(input))
    print("Part 2:", pw2(input))
