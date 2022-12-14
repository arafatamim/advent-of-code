size = 1000
grid = [[0 for _ in range(size)] for _ in range(size)]


def parse_instr(str: str):
    if str.startswith("turn on") or str.startswith("turn off"):
        split = str.split(" ")
        op = split[1]
        x1, y1 = [int(x) for x in split[2].split(",")]
        x2, y2 = [int(x) for x in split[4].split(",")]
        return {"from": (x1, y1), "to": (x2, y2), "op": op}
    elif str.startswith("toggle"):
        split = str.split(" ")
        x1, y1 = [int(x) for x in split[1].split(",")]
        x2, y2 = [int(x) for x in split[3].split(",")]
        return {"from": (x1, y1), "to": (x2, y2), "op": "toggle"}
    else:
        raise Exception()


def flip_lights(instruction):
    x1, y1 = instruction["from"]
    x2, y2 = instruction["to"]
    op = instruction["op"]

    for x in range(x1, x2 + 1):
        for y in range(y1, y2 + 1):
            if op == "on":
                grid[x][y] = 1
            elif op == "off":
                grid[x][y] = 0
            elif op == "toggle":
                grid[x][y] = 1 - grid[x][y]


def regulate_lights(instruction):
    x1, y1 = instruction["from"]
    x2, y2 = instruction["to"]
    op = instruction["op"]

    for x in range(x1, x2 + 1):
        for y in range(y1, y2 + 1):
            if op == "on":
                grid[x][y] += 1
            elif op == "off":
                grid[x][y] = max(0, grid[x][y] - 1)
            elif op == "toggle":
                grid[x][y] += 2


def count_lights():
    count = 0
    for x in grid:
        for y in x:
            if y == 1:
                count += 1
    return count


def count_brightness():
    count = 0
    for x in grid:
        for y in x:
            count += y
    return count


file = open("input.txt", "r")
input = [parse_instr(line) for line in file.readlines()]

# Part I
for instr in input:
    flip_lights(instr)
print(count_lights())

# reset grid
grid = [[0 for _ in range(size)] for _ in range(size)]

# Part II
for instr in input:
    regulate_lights(instr)
print(count_brightness())
