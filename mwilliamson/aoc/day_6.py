import io
import os


def main():
    assert _count_orbits(_read_input(io.StringIO("""\
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"""))) == 42

    with open(os.path.join(os.path.dirname(__file__), "day_6.txt")) as fileobj:
        print(_count_orbits(_read_input(fileobj)))


def _count_orbits(direct_orbits):
    count = 0

    for orbitter, centre in direct_orbits.items():
        while centre is not None:
            count += 1
            centre, orbitter = direct_orbits.get(centre), centre

    return count


def _read_input(fileobj):
    return dict([
        reversed(line.strip().split(")", 1))
        for line in fileobj
    ])


if __name__ == "__main__":
    main()
