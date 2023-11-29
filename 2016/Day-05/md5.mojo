from python import Python
from memory import memset_zero

alias S = StaticIntTuple[64](
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
)

alias T = StaticIntTuple[64](
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
)

alias A = 0x67452301
alias B = 0xefcdab89
alias C = 0x98badcfe
alias D = 0x10325476

struct Md5Ctxt:
    var size: UInt64
    var buffer: DTypePointer[DType.uint32]
    var input: DTypePointer[DType.uint8]
    var digest: DTypePointer[DType.uint8]

    fn __init__ (inout self):
        self.size = 0
        self.buffer = self.buffer.alloc(4)
        self.input = self.input.alloc(64)
        self.digest = self.digest.alloc(16)

        self.buffer.store(0, A)
        self.buffer.store(1, B)
        self.buffer.store(2, C)
        self.buffer.store(3, D)

    fn hexdigest (self) -> String:
        alias int_to_hex = StaticIntTuple[16](
            ord('0'), ord('1'), ord('2'), ord('3'), 
            ord('4'), ord('5'), ord('6'), ord('7'), 
            ord('8'), ord('9'), ord('a'), ord('b'), 
            ord('c'), ord('d'), ord('e'), ord('f') 
        )

        var hex_str: String = ""
        for i in range(16):
            let top = self.digest.load(i) >> 4
            let bottom = self.digest.load(i) & 0x0F 
            hex_str += (chr(int_to_hex[top.to_int()]) + chr(int_to_hex[bottom.to_int()]))

        return hex_str


fn F (x: UInt32, y: UInt32, z: UInt32) -> UInt32:
    return (x & y) | ((~x) & z)

fn G (x: UInt32, y: UInt32, z: UInt32) -> UInt32:
    return (x & z) | (y & (~z))

fn H (x: UInt32, y: UInt32, z: UInt32) -> UInt32:
    return x ^ y ^ z

fn I (x: UInt32, y: UInt32, z: UInt32) -> UInt32:
    return y ^ (x | (~z))


fn rotate_left (x: UInt32, n: UInt32) -> UInt32:
    return (x << n) | (x >> (32 - n))

fn to_little_endian (buffer: DTypePointer[DType.uint8], idx: Int) -> UInt32:
    let _0 = SIMD[DType.uint32, 1](buffer.load(idx).to_int())
    let _1 = SIMD[DType.uint32, 1](buffer.load(idx + 1).to_int())
    let _2 = SIMD[DType.uint32, 1](buffer.load(idx + 2).to_int())
    let _3 = SIMD[DType.uint32, 1](buffer.load(idx + 3).to_int())

    return (_3 << 24) | (_2 << 16) | (_1 << 8) | _0


fn md5_step(buffer: DTypePointer[DType.uint32], input: DTypePointer[DType.uint32]):
    var AA = buffer.load(0)
    var BB = buffer.load(1)
    var CC = buffer.load(2)
    var DD = buffer.load(3)

    let E: UInt32
    let j: UInt32

    for i in range(64):
        let i_div16 = i // 16

        if i_div16 == 0:
            E = F(BB, CC, DD)
            j = i
        elif i_div16 == 1:
            E = G(BB, CC, DD)
            j = ((i * 5) + 1) % 16
        elif i_div16 == 2:
            E = H(BB, CC, DD)
            j = ((i * 3) + 5) % 16
        else:
            E = I(BB, CC, DD)
            j = (i * 7) % 16

        let temp = DD
        DD = CC
        CC = BB
        BB = BB + rotate_left(AA + E + T[i] + input.load(j.to_int()), S[i])
        AA = temp
    
    buffer.store(0, buffer.load(0) + AA)
    buffer.store(1, buffer.load(1) + BB)
    buffer.store(2, buffer.load(2) + CC)
    buffer.store(3, buffer.load(3) + DD)


fn md5_update(inout ctxt: Md5Ctxt, input_buffer: DTypePointer[DType.uint8], input_len: UInt64):
    let input = DTypePointer[DType.uint32].alloc(16)
    var offset = ctxt.size % 64
    ctxt.size += input_len

    var i: UInt64 = 0
    while i < input_len:
        ctxt.input.store(offset.to_int(), input_buffer.load(i.to_int()))
        offset += 1

        if offset % 64 == 0:
            var j: UInt64 = 0
            while j < 16: 
                input.store(j.to_int(), to_little_endian(ctxt.input, (j * 4).to_int()))
                j += 1

            md5_step(ctxt.buffer, input)
            offset = 0

        i += 1


fn md5_finalize(inout ctxt: Md5Ctxt):
    let input = DTypePointer[DType.uint32].alloc(16)
    let offset = ctxt.size % 64
    let pad_len = 56 - offset if offset < 56 else (56 + 64) - offset

    alias padding = DTypePointer[DType.uint8].alloc(64)
    memset_zero(padding, 64)
    padding.store(0, 0x80)

    md5_update(ctxt, padding, pad_len)
    ctxt.size -= pad_len

    var i: UInt64 = 0
    while i < 14:
        input.store(i.to_int(), to_little_endian(ctxt.input, (i * 4).to_int()))
        i += 1
    
    input.store(14, SIMD[DType.uint32, 1](ctxt.size.to_int() * 8))
    input.store(15, SIMD[DType.uint32, 1]((ctxt.size.to_int() * 8) >> 32))

    md5_step(ctxt.buffer, input)

    var j: UInt64 = 0
    while j < 4:
        let _a = ctxt.buffer.load(j.to_int()) & 0x000000FF
        let _b = (ctxt.buffer.load(j.to_int()) & 0x0000FF00) >> 8
        let _c = (ctxt.buffer.load(j.to_int()) & 0x00FF0000) >> 16
        let _d = (ctxt.buffer.load(j.to_int()) & 0xFF000000) >> 24

        ctxt.digest.store((j * 4).to_int(), SIMD[DType.uint8, 1](_a.to_int()))
        ctxt.digest.store((j * 4 + 1).to_int(), SIMD[DType.uint8, 1](_b.to_int()))
        ctxt.digest.store((j * 4 + 2).to_int(), SIMD[DType.uint8, 1](_c.to_int()))
        ctxt.digest.store((j * 4 + 3).to_int(), SIMD[DType.uint8, 1](_d.to_int()))

        j += 1


fn md5_str_literal(str: StringLiteral) -> String:
    var ctxt = Md5Ctxt()

    let uint8_input = DTypePointer[DType.int8](str.data()).bitcast[DType.uint8]()

    md5_update(ctxt, uint8_input, len(str))
    md5_finalize(ctxt)

    return ctxt.hexdigest()


fn md5(str: String) -> String:
    let uint8_input = DTypePointer[DType.uint8].alloc(len(str))
    for i in range(len(str)):
        uint8_input.store(i, SIMD[DType.uint8, 1](ord(str[i])))

    var ctxt = Md5Ctxt()

    md5_update(ctxt, uint8_input, len(str))
    md5_finalize(ctxt)

    return ctxt.hexdigest()


fn md5_tests ():
    debug_assert(md5("") == "d41d8cd98f00b204e9800998ecf8427e", "md5 test 1 failed")
    debug_assert(md5("a") == "0cc175b9c0f1b6a831c399e269772661", "md5 test 2 failed")
    debug_assert(md5("abc") == "900150983cd24fb0d6963f7d28e17f72", "md5 test 3 failed")
    debug_assert(md5("message digest") == "f96b697d7cb7938d525a2f31aaf161d0", "md5 test 4 failed")
    debug_assert(md5("abcdefghijklmnopqrstuvwxyz") == "c3fcd3d76192e4007dfb496cca67e13b", "md5 test 5 failed")
    debug_assert(md5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") == "d174ab98d277d9f5a5611c2c9f419d9f", "md5 test 6 failed")
    debug_assert(md5("12345678901234567890123456789012345678901234567890123456789012345678901234567890") == "57edf4a22be3c955ac49da2e2107b67a", "md5 test 7 failed")

    debug_assert(md5_str_literal("") == "d41d8cd98f00b204e9800998ecf8427e", "md5 test 1 failed")
    debug_assert(md5_str_literal("a") == "0cc175b9c0f1b6a831c399e269772661", "md5 test 2 failed")
    debug_assert(md5_str_literal("abc") == "900150983cd24fb0d6963f7d28e17f72", "md5 test 3 failed")
    debug_assert(md5_str_literal("message digest") == "f96b697d7cb7938d525a2f31aaf161d0", "md5 test 4 failed")
    debug_assert(md5_str_literal("abcdefghijklmnopqrstuvwxyz") == "c3fcd3d76192e4007dfb496cca67e13b", "md5 test 5 failed")
    debug_assert(md5_str_literal("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") == "d174ab98d277d9f5a5611c2c9f419d9f", "md5 test 6 failed")
    debug_assert(md5_str_literal("12345678901234567890123456789012345678901234567890123456789012345678901234567890") == "57edf4a22be3c955ac49da2e2107b67a", "md5 test 7 failed")


fn md5_py(data: String) raises -> String:
    let py_hashlib: PythonObject = Python.import_module("hashlib")
    return py_hashlib.md5(PythonObject(data).encode()).hexdigest().to_string()

fn main() raises:
    md5_tests()
    print("All tests passed!")
