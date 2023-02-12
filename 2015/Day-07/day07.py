import sys
import re

reg: dict[str, tuple[str, str, str]] = {}
insts: list[str] = sys.stdin.read().strip().split('\n')

gen_b_op_reg = lambda op_str : f'(\\w+) {op_str} (\\w+) -> (\\w+)'

for inst in insts:
    if "AND" in inst:
        a_op1, a_op2, a_reg = re.compile(gen_b_op_reg("AND")).match(inst).groups()
        reg[a_reg] = ("AND", a_op1, a_op2)
    elif "OR" in inst:
        o_op1, o_op2, o_reg = re.compile(gen_b_op_reg("OR")).match(inst).groups()
        reg[o_reg] = ("OR", o_op1, o_op2)
    elif "LSHIFT" in inst:
        l_op1, l_op2, l_reg = re.compile(gen_b_op_reg("LSHIFT")).match(inst).groups()
        reg[l_reg] = ("LSHIFT", l_op1, l_op2)
    elif "RSHIFT" in inst:
        r_op1, r_op2, r_reg = re.compile(gen_b_op_reg("RSHIFT")).match(inst).groups()
        reg[r_reg] = ("RSHIFT", r_op1, r_op2)
    elif "NOT" in inst:
        n_op1, n_reg = re.compile('NOT (\\w+) -> (\\w+)').match(inst).groups()
        reg[n_reg] = ("NOT", n_op1, "")
    else:
        aa_op, aa_reg = re.compile('(\\w+) -> (\\w+)').match(inst).groups() 
        reg[aa_reg] = ("->", aa_op, "")

cache: dict[str, int] = dict()
def find_val(wire: str) -> int:
    if (ord(wire[0]) < 65): return int(wire)
    if (wire in cache): return cache[wire]
    
    res: int = -1
    op_name, op1, op2 = reg[wire]
    if op_name == "->":
        res = find_val(op1)
    elif op_name == "NOT":
        res = 65535 + ~find_val(op1) + 1
    elif op_name == "AND":
        res = find_val(op1) & find_val(op2)
    elif op_name == "OR":
        res = find_val(op1) | find_val(op2)
    elif op_name == "LSHIFT":
        res = find_val(op1) << int(op2)
    elif op_name == "RSHIFT":
        res = find_val(op1) >> int(op2)
    
    cache[wire] = res;
    return res;

a_val: int = find_val('a')
print(f"Part 1: {a_val}")

cache = dict() # reset cache
cache['b'] = a_val
print(f"Part 2: {find_val('a')}")

