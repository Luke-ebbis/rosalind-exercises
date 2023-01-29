#! /usr/bin/env python
"""Rabbits and their recurrence relations

:author: Sibbe Bakker
:description: Solution to the recursive rabbits problem from rosalind (problem
    link)[https://rosalind.info/problems/fib/].
:example session: $ ./fib.py 5 3
                  19
:usage:  ./name -filter          Take and give to standard io
         ./name file             Take from file, give to stout
         ./name file -filter     Take from standard in, give to file
         ./name file1 file2      Take from file1, write to file two
"""
import sys, os

arguments = sys.argv[1:]


def calculate_rabbit_population(n: int, k: int) -> int:
    """Calculate the population of recursive rabbits.

    :n: int: The number of generations.
    :k: int: The breeding rate of each rabbit pair.
    :return: int: The number of rabbits after n generations with a breeding
        rate of k.
    """
    n_current = 1
    population = [1, 0]
    while n_current < n:
        population[1] = population[0] + population[1] * k
        current_population = population[1]
        population.insert(0, current_population)
        # print(f"# {n_current}\t {population[2]}")
        n_current += 1

    return population[0]


def main():
    """The main procedure
 
    :return: Nothing
    """
    if len(sys.argv[1:]) != 2:
        print(f"usage: {sys.argv[0]} <n> <k> \n"
              f"n \t The number of generations. \n"
              f"k \t The breeding rate of the recursive rabbits.")
        exit(1)
    n, k = [int(x) for x in sys.argv[1:3]]
    population = calculate_rabbit_population(n, k)
    print(population, file=sys.stdout)
    exit(0)


if __name__ == "__main__":
    main()
