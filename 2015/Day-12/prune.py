import json


def remove_red(data):
    if isinstance(data, dict):
        if 'red' in data.values():
            return {}
        else:
            return {k: remove_red(v) for k, v in data.items()}
    elif isinstance(data, list):
        return [remove_red(x) for x in data]

    return data

if __name__ == "__main__":
    data = json.load(open('input.txt'))
    json.dump(remove_red(data), open('pruned-input.txt', 'w'))
