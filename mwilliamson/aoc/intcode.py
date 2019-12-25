def run(program):
    program = list(program)

    position = 0
    while True:
        opcode = program[position]
        if opcode == 99:
            return program

        src_1 = program[program[position + 1]]
        src_2 = program[program[position + 2]]

        if opcode == 1:
            result = src_1 + src_2
        elif opcode == 2:
            result = src_1 * src_2

        program[program[position + 3]] = result
        position += 4
