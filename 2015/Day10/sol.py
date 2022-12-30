input = "3113322113"


def groupby(input):
    new_list = []
    for value in input:
        if new_list and new_list[-1][0] == value:
            new_list[-1].append(value)
        else:
            new_list.append([value])
    return new_list


def look_and_say(input, repeat):
    result = ""
    for _ in range(repeat):
        for x in groupby(input):
            length = len(x)
            item = x[0]
            result += str(length) + item
        input = result
        result = ""
    return len(input)


print(look_and_say(input, 40))
