file = open("input.txt", "r")
banks = [int(a) for a in file.read().strip().split("\t")]

# banks = [0, 2, 7, 0]

states = []


cycles = 0
while True:
    largest_i, largest_el = max(enumerate(banks), key=lambda k: (k[1], -k[0]))

    banks[largest_i] = 0

    cursor = largest_i
    while largest_el > 0:
        cursor = (cursor + 1) % len(banks)
        banks[cursor] += 1
        largest_el -= 1

    cycles += 1

    if banks in states:
        break
    else:
        states.append(banks.copy())

print(cycles)
print(len(states)-states.index(banks))
