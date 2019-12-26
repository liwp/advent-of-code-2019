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

    assert _count_transfers(_read_input(io.StringIO("""\
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
K)L
K)YOU
I)SAN"""))) == 4

    with open(os.path.join(os.path.dirname(__file__), "day_6.txt")) as fileobj:
        direct_orbits = _read_input(fileobj)
        print(_count_orbits(direct_orbits))
        print(_count_transfers(direct_orbits))


def _count_orbits(direct_orbits):
    count = 0

    for orbitter, centre in direct_orbits.items():
        while centre is not None:
            count += 1
            centre, orbitter = direct_orbits.get(centre), centre

    return count


def _count_transfers(direct_orbits):
    you_distances = {}

    body = direct_orbits["YOU"]
    while body is not None:
        you_distances[body] = len(you_distances)
        body = direct_orbits.get(body)

    santa_distances = {}

    body = direct_orbits["SAN"]
    while body is not None:
        santa_distances[body] = len(santa_distances)
        body = direct_orbits.get(body)

    return min(
        you_distances[body] + santa_distances[body]
        for body in you_distances.keys()
        if body in santa_distances
    )


def _read_input(fileobj):
    return dict([
        reversed(line.strip().split(")", 1))
        for line in fileobj
    ])


if __name__ == "__main__":
    main()
