def run(program, *, next_input=None):
    program = list(program)

    position = 0
    while True:
        opcode = program[position]
        if opcode == 99:
            return program

        if opcode in (1, 2):
            src_1 = program[program[position + 1]]
            src_2 = program[program[position + 2]]

            if opcode == 1:
                result = src_1 + src_2
            elif opcode == 2:
                result = src_1 * src_2

            program[program[position + 3]] = result
            position += 4
        elif opcode == 3:
            program[program[position + 1]] = next_input()
            position += 2
