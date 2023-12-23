#! /usr/bin/env python
"""Calculating the hamming distance
"""


import sys


def main():
    """main."""
    if len(sys.argv[1:]) != 1:
        print(f"usage: {sys.argv[0]} <file> \n"
              f"file \t File containing two sequences.")
        sys.exit(1)
    file_path = sys.argv[1]
    with open(file_path, "r") as f:
        data = f.read()
    lines = data.split("\n")
    seq1, seq2 = [x for x in lines if x != ""]
    print(hamming_distance(seq1, seq2), file=sys.stdout)


def hamming_distance(x: str, y: str) -> int:
    """hamming_distance.

    :param x: Sequence x
    :type x: str
    :param y: Sequence y
    :type y: str
    :rtype: int
    """
    assert len(x) == len(y),\
        "The hamming distance is not defined for sequences of unequal length."
    differences = [1 for base_x, base_y in zip(x, y) if base_x != base_y]
    d_hamming = sum(differences)
    return d_hamming


if __name__ == "__main__":
    main()
