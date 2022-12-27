file = open("input.txt", "r")
input = file.read().strip()

def part_1():
    sum = 0
    for i, s in enumerate(input):
        next_index = (i + 1) % len(input)
        if s == input[next_index]:
            sum += int(s)
    print(sum)


def part_2():
    sum = 0
    half_index = int(len(input) / 2)
    for i, s in enumerate(input):
        next_index = (i + half_index) % len(input)
        if s == input[next_index]:
            sum += int(s)
    print(sum)


part_1()
part_2()
