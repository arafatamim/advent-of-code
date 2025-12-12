dial = 50

file = open("input.txt", "r")

num_zeroes = 0

for line in file:
    direction = line[0]
    magnitude = int(line[1:])

    if direction == "L":
        dial = (dial - magnitude) % 100
    elif direction == "R":
        dial = (dial + magnitude) % 100

    if dial == 0:
        num_zeroes += 1


print(f"Number of zeroes: {num_zeroes}")

file.seek(0)
dial = 50

num_zeroes = 0

for line in file:
    rotate_by = int(line[1:])
    for i in range(rotate_by):
        if line[0] == "L":
            dial = dial - 1
        elif line[0] == "R":
            dial = dial + 1

        if dial % 100 == 0:
            num_zeroes += 1

print(f"Number of zeroes dial passes: {num_zeroes}")
