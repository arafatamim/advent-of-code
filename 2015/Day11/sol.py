def groupby(list):
    new_list = []
    for value in list:
        if new_list and new_list[-1][0] == value:
            new_list[-1].append(value)
        else:
            new_list.append([value])
    return new_list


def has_consecutive(password):
    return any(
        ord(password[i + 1]) == ord(password[i]) + 1
        and ord(password[i + 2]) == ord(password[i]) + 2
        for i in range(0, len(password) - 2)
    )


def has_doubles(password):
    return len([x for x in groupby(password) if len(x) >= 2]) >= 2


def has_forbidden(password):
    return any(bad_letter in password for bad_letter in ["i", "o", "l"])


def is_valid_password(password):
    return (
        has_consecutive(password)
        and has_doubles(password)
        and not has_forbidden(password)
    )


def increment_password(password):
    r = list(password)[::-1]
    i = 0
    for c in r:
        if c == "z":
            r[i] = "a"
        else:
            r[i] = chr(ord(c) + 1)
            break
        i += 1
    return "".join(r[::-1])


def find_new_password(string):
    new_password = increment_password(string)
    while not is_valid_password(new_password):
        new_password = increment_password(new_password)
    return new_password


password = find_new_password("vzbxkghb")
print(password)

print(find_new_password(password))
