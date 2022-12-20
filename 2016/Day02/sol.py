file = open("input.txt", "r")
# instructions = ["ULL", "RRDDD", "LURDL", "UUUUD"]
instructions = file.readlines()


def exec_instructions(keypad, ix, iy):
    for instr in instructions:
        for char in instr:
            match char:
                case "U":
                    if iy > 0 and keypad[iy - 1][ix] != "x":
                        iy -= 1
                case "D":
                    if iy < len(keypad) - 1 and keypad[iy + 1][ix] != "x":
                        iy += 1
                case "L":
                    if ix > 0 and keypad[iy][ix - 1] != "x":
                        ix -= 1
                case "R":
                    if ix < len(keypad[iy]) - 1 and keypad[iy][ix + 1] != "x":
                        ix += 1
        print(keypad[iy][ix], end="")


keypad1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
exec_instructions(keypad1, 1, 1)

print()

keypad2 = [
    "xx1xx",  # 0
    "x234x",  # 1
    "56789",  # 2
    "xABCx",  # 3
    "xxDxx",  # 4
]
exec_instructions(keypad2, 0, 2)

print()
