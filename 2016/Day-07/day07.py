import sys

data = sys.stdin.read().strip().split('\n')

def is_ip7(ip: str) -> bool:
    has_seq: bool = False
    has_hypernet_seq: bool = False

    i: int = 0
    in_hypernet: bool = False
    while i <= len(ip) - 4:
        if in_hypernet and ip[i] == "]":
            in_hypernet = False
            i += 1
            continue
        if not in_hypernet and ip[i] == "[":
            in_hypernet = True
            i += 1
            continue
        
        if ip[i] != ip[i+1] and ip[i] == ip[i+3] and ip[i+1] == ip[i+2]:
            has_seq = has_seq or (not in_hypernet)
            has_hypernet_seq = has_hypernet_seq or in_hypernet
        i += 1

    return has_seq and (not has_hypernet_seq)

def is_ssl(ip: str) -> bool:
    seqs = set()
    hypernet_seqs = set()

    i: int = 0
    in_hypernet: bool = False
    while i <= len(ip) - 3:
        if in_hypernet and ip[i] == "]":
            in_hypernet = False
            i += 1
            continue
        if not in_hypernet and ip[i] == "[":
            in_hypernet = True
            i += 1
            continue
        
        if ip[i] == ip[i+2] and ip[i] != ip[i+1]:
            if in_hypernet:
                hypernet_seqs.add(f"{ip[i+1]}{ip[i]}{ip[i+1]}")
            else:
                seqs.add(ip[i:i+3])
        i += 1

    return len(seqs.intersection(hypernet_seqs)) > 0


num_ip7s: int = sum(map(is_ip7, data))
print(f"Part 1: {num_ip7s}")

num_ssls: int = sum(map(is_ssl, data))
print(f"Part 2: {num_ssls}")

