def main():
    _part_1()
    _part_2()


def _part_1():
    print("part 1")

    def is_possible_password(number):
        return _has_same_adjacent_digits(number) and _digits_never_decrease(number)

    assert is_possible_password("111111")
    assert not is_possible_password("223450")
    assert not is_possible_password("123789")

    count = 0

    for number in range(245182, 790572 + 1):
        if is_possible_password(str(number)):
            count += 1

    print(count)


def _part_2():
    print("part 2")

    def is_possible_password(number):
        return _has_same_adjacent_isolated_digits(number) and _digits_never_decrease(number)

    assert not is_possible_password("111111")
    assert not is_possible_password("223450")
    assert not is_possible_password("123789")


    assert is_possible_password("112233")
    assert not is_possible_password("123444")
    assert is_possible_password("111122")

    count = 0

    for number in range(245182, 790572 + 1):
        if is_possible_password(str(number)):
            count += 1

    print(count)


def _has_same_adjacent_isolated_digits(number):
    return any(
        (
            number[index] == number[index + 1] and
            number[index] != _get_or_none(number, index - 1) and
            number[index] != _get_or_none(number, index + 2)
        )
        for index in range(len(number) - 1)
    )


def _has_same_adjacent_digits(number):
    return any(
        number[index] == number[index + 1]
        for index in range(len(number) - 1)
    )


def _digits_never_decrease(number):
    return all(
        number[index] <= number[index + 1]
        for index in range(len(number) - 1)
    )


def _get_or_none(value, index):
    try:
        return value[index]
    except IndexError:
        return None


if __name__ == "__main__":
    main()
