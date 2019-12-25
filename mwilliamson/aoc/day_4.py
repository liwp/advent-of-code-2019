def main():
    assert _is_possible_password("111111")
    assert not _is_possible_password("223450")
    assert not _is_possible_password("123789")

    count = 0

    for number in range(245182, 790572 + 1):
        if _is_possible_password(str(number)):
            count += 1

    print(count)


def _is_possible_password(number):
    return _has_same_adjacent_digits(number) and _digits_never_decrease(number)


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


if __name__ == "__main__":
    main()
