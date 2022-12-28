file = open("input.txt", 'r')
maze = [int(line) for line in file.read().strip().split("\n")]
# maze = [0, 3, 0, 1, -3]

def solve(maze):
    cursor = 0
    steps = 0

    while 0 <= cursor < len(maze):
        cur_instr = maze[cursor]
        if cur_instr >= 3:
            maze[cursor] -= 1
        else:
            maze[cursor] += 1
        cursor += cur_instr
        steps += 1

    return steps

# print("final maze: ", data)
print("escaped from maze in %d steps" % solve(maze))
