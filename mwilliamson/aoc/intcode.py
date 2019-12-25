def run(program, *, next_input=None, output=None):
    state = State(program)

    while True:
        opcode = state.opcode()

        if opcode == 99:
            return state.memory

        elif opcode in (1, 2):
            src_1 = state.read_parameter(0)
            src_2 = state.read_parameter(1)

            if opcode == 1:
                result = src_1 + src_2
            elif opcode == 2:
                result = src_1 * src_2

            state.write_parameter(2, result)
            state.instruction_pointer += 4

        elif opcode == 3:
            state.write_parameter(0, next_input())
            state.instruction_pointer += 2

        elif opcode == 4:
            output(state.read_parameter(0))
            state.instruction_pointer += 2

        elif opcode == 5:
            if state.read_parameter(0) == 0:
                state.instruction_pointer += 3
            else:
                state.instruction_pointer = state.read_parameter(1)

        else:
            raise ValueError("unknown opcode: {}".format(opcode))


class State(object):
    def __init__(self, memory):
        self.memory = list(memory)
        self.instruction_pointer = 0

    def opcode_value(self):
        return self.memory[self.instruction_pointer]

    def opcode(self):
        return self.opcode_value() % 100

    def read_parameter(self, index):
        parameter_mode = (self.opcode_value() // 100 // 10 ** index) % 10
        immediate = self._read_immediate_parameter(index)
        if parameter_mode == 0:
            return self.memory[immediate]
        elif parameter_mode == 1:
            return immediate
        else:
            raise ValueError("unknown parameter mode: {}".format(parameter_mode))

    def write_parameter(self, index, value):
        self.memory[self._read_immediate_parameter(index)] = value

    def _read_immediate_parameter(self, index):
        return self.memory[self.instruction_pointer + index + 1]
