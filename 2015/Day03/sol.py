file = open("input.txt", "r")
directions = file.read()

# Part I
def part1():
    cur: list[tuple[int, int]] = [(0, 0)]
    for dir in directions:
        x, y = cur[-1]
        if dir == "^":
            cur.append((x, y - 1))
        elif dir == "v":
            cur.append((x, y + 1))
        elif dir == ">":
            cur.append((x + 1, y))
        elif dir == "<":
            cur.append((x - 1, y))

    print(len(set(cur)))


# Part II

def part2():
    cur: dict[str, list[tuple[int, int]]] = {"santa": [(0, 0)], "robot": [(0, 0)]}
    turn = "santa"
    for dir in directions:
        x, y = cur[turn][-1]
        if dir == "^":
            cur[turn].append((x, y - 1))
        elif dir == "v":
            cur[turn].append((x, y + 1))
        elif dir == ">":
            cur[turn].append((x + 1, y))
        elif dir == "<":
            cur[turn].append((x - 1, y))

        if turn == "robot":
            turn = "santa"
        else:
            turn = "robot"

    houses_visited = len(set(cur["santa"] + cur["robot"]))
    print(houses_visited)


part1()

part2()
