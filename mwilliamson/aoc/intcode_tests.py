from .intcode import run


_JUNK = 72984721984719824


def test_halt():
    result = run([99])

    assert result == [99]


def test_add_adds_together_numbers_from_two_addresses_and_stores_in_third_address():
    result = run([
        1, 6, 9, 12,
        99,
        _JUNK,
        20,
        _JUNK,
        _JUNK,
        7,
        _JUNK,
        _JUNK,
        42,
    ])

    assert result == [
        1, 6, 9, 12,
        99,
        _JUNK,
        20,
        _JUNK,
        _JUNK,
        7,
        _JUNK,
        _JUNK,
        27,
    ]


def test_multiply_multiplies_together_numbers_from_two_addresses_and_stores_in_third_address():
    result = run([
        2, 6, 9, 12,
        99,
        _JUNK,
        20,
        _JUNK,
        _JUNK,
        7,
        _JUNK,
        _JUNK,
        42,
    ])

    assert result == [
        2, 6, 9, 12,
        99,
        _JUNK,
        20,
        _JUNK,
        _JUNK,
        7,
        _JUNK,
        _JUNK,
        140,
    ]


def test_input_takes_input_and_saves_to_address():
    inputs = [44, 43, 42]

    def next_input():
        return inputs.pop()

    result = run([
        3, 7,
        3, 8,
        3, 9,
        99,
        _JUNK,
        _JUNK,
        _JUNK,
    ], next_input=next_input)

    assert result == [
        3, 7,
        3, 8,
        3, 9,
        99,
        42,
        43,
        44,
    ]


def test_output_reads_address_and_sends_value_to_output():
    outputs = []

    result = run([
        4, 7,
        4, 8,
        4, 9,
        99,
        42,
        43,
        44,
    ], output=outputs.append)

    assert outputs == [42, 43, 44]

class TestJumpIfTrue(object):
    def test_when_first_parameter_is_nonzero_then_instruction_pointer_is_set_to_second_parameter(self):
        outputs = []

        result = run([
            1105, 1, 5,
            104, 42,
            99,
        ], output=outputs.append)

        assert outputs == []

    def test_when_first_parameter_is_zero_then_instruction_pointer_is_set_to_next_instruction(self):
        outputs = []

        result = run([
            1105, 0, 5,
            104, 42,
            99,
        ], output=outputs.append)

        assert outputs == [42]


class TestJumpIfFalse(object):
    def test_when_first_parameter_is_zero_then_instruction_pointer_is_set_to_second_parameter(self):
        outputs = []

        result = run([
            1106, 1, 5,
            104, 42,
            99,
        ], output=outputs.append)

        assert outputs == [42]

    def test_when_first_parameter_is_nonzero_then_instruction_pointer_is_set_to_next_instruction(self):
        outputs = []

        result = run([
            1106, 0, 5,
            104, 42,
            99,
        ], output=outputs.append)

        assert outputs == []


class TestLessThan(object):
    def test_when_first_parameter_is_less_than_second_parameter_then_one_is_stored_in_third_parameter(self):
        result = run([
            1107, 1, 2, 5,
            99,
            _JUNK,
        ])

        assert result == [
            1107, 1, 2, 5,
            99,
            1,
        ]

    def test_when_first_parameter_is_equal_to_second_parameter_then_zero_is_stored_in_third_parameter(self):
        result = run([
            1107, 2, 2, 5,
            99,
            _JUNK,
        ])

        assert result == [
            1107, 2, 2, 5,
            99,
            0,
        ]

    def test_when_first_parameter_is_greater_than_second_parameter_then_zero_is_stored_in_third_parameter(self):
        result = run([
            1107, 3, 2, 5,
            99,
            _JUNK,
        ])

        assert result == [
            1107, 3, 2, 5,
            99,
            0,
        ]


class TestParameterModes(object):
    def test_parameter_mode_zero_treats_parameters_as_addresses(self):
        result = run([
            1, 5, 6, 7,
            99,
            20,
            7,
            42,
        ])

        assert result == [
            1, 5, 6, 7,
            99,
            20,
            7,
            27,
        ]

    def test_parameter_mode_one_treats_parameters_as_addresses(self):
        result = run([
            1101, 5, 6, 7,
            99,
            20,
            7,
            42,
        ])

        assert result == [
            1101, 5, 6, 7,
            99,
            20,
            7,
            11,
        ]
