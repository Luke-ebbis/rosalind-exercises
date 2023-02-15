#! /usr/bin/env python
"""Script for the calculation of Mendels first law.
author --- Sibbe Bakker
date ---  Tue 07 Feb 2023 08:12:52 PM CET
theory --- In a population there is mating of genetic individuals. Using
        Mendels law it is possible to predict how a genotype might spread in a
        population. In this script, the spread of a genotype throughout a
        population can be predicted using a formula and a simulation.
            In a population there can be dominant (k), heterozygous (m) or
        homozygous recessive (n) number of individuals.
usage      This script is used to calculate the chance of a dominant allele
        in a population using three numbers as input: k, m and n:
       
        ```bash
            ./iprb.py <k> <m> <n>

            ./iprb.py 2 2 2
            0.78333
        ```
rosalind --- https://rosalind.info/problems/iprb/
sources --- http://saradoesbioinformatics.blogspot.com/2016/06/mendels-first-
    law.html.
todo --- The solution will be based on a simulation. This simulation does not
        obtain the correct answers. Next I will try with a probability tree.
"""

import sys
import random


def main():
    """The main procedure

    :return: Nothing
    """
    # Argument handling
    if len(sys.argv[1:]) != 3:
        print(
            f"usage: {sys.argv[0]} <k> <m> <n> \n"
            f"k \t The number of homozygously dominant individuals.\n"
            f"m \t The number of heterozygous individuals.\n"
            f"n \t The number of recessive individuals. "
        )
        sys.exit(1)
    # Unpacking the three main variables for this script.
    k, m, n = [int(x) for x in sys.argv[1:4]]

    # Calculations
    # chance_dominant = simulation_dominant_phenotype(k, m, n, times=2000)
    # I got the formula from sara; I need to figure out how this works; mendels
    # law is difficult for me.
    pop = sum([k, m, n])
    chance_dominant = (4*(k*(k-1)+2*k*m+2*k*n+m*n)+3*m*(m-1))/(4*pop*(pop-1))
    # chance_dominant = probability_tree(k, m, n,
    #                                    offspring_genotype=("k", "m",))
    print(chance_dominant, file=sys.stdout)
    sys.exit(0)


def simulation_dominant_phenotype(k, m, n, times=100) -> float:
    """Simulating the chance that a random mating leads to a dominant phenotype

    note --- This does not seem to obtain the correct answer from the rosalind
            example.
    """
    outcomes = list()
    for i in range(times):
        population = Population(n, k, n)
        population_after_mating = population.mating_probability()
        outcome_i = (
            population_after_mating["heterozygous"]
            + population_after_mating["homozygous dominant"]
        )
        outcomes.append(outcome_i)
    chance_dominant = mean(outcomes)
    return chance_dominant


# Utilities ----

def mean(x: iter) -> float:
    """Calculating the mean of a array of numbers.

    :param x: iter: An array of numbers of which the mean must be determined.
    :return: float: The mean of the array x.
    """
    return sum(x) / len(x)


# Probability tree
def probability_tree(k, m, n, offspring_genotype=("k", "m",)) -> float:
    """Obtain the chances of a random offspring genotype.

    :param k: int: The number of dominant individuals in the population.
    :param m: int: The number of heterozygous individuals in the population.
    :param n: int: The number of homozygous individuals in the population.
    :param offspring_genotype: tuple: Which genotype the chance should be
        calculated for in the offspring population. Can be a tuple containing
        strings of "k", "m" and "n".
    :return: float: The chance of obtaining the specified genotype though a
        random mating event.
    theory --- To obtain the chance of selecting to mating partners the chance
        of selecting any one of them must be multiplied.
    linkout --- http://saradoesbioinformatics.blogspot.com/2016/06/mendels-
        first-law.html
    """
    specified = set(offspring_genotype)
    assert specified.issubset({'k', 'n', 'm'}),\
        "offspring_genotype not correct"
    pass


# Simulation studies
class Individual:
    def __init__(self, genotype):
        """An individual organism
        :param genotype: The genotype of the organism. Can be homozygous
            dominant, homozygous recessive or heterozygous.
        """
        assert genotype in {
            "homozygous dominant",
            "homozygous recessive",
            "heterozygous",
        }, (
            f"{genotype} is not in the set homozygous dominant, recessive or "
            f"heterozygous."
        )

        self.genotype = genotype

        # The identifier code of this instance.
        self.id_code = id(self)

    def mate(self, other, split_probability=0.5):
        """Mate two individuals and return the offspring based on chance
        :param self: Individual: The self.
        :param other: Individual: The other individual.
        :return: Individual: A new individual with a new random genotype.
        """
        assert isinstance(other, Individual), "Class mismatch."
        state_number = split_probability**-1
        state = random.randint(0, state_number)
        if state > state_number / 2:
            offspring = Individual(genotype=self.genotype)
        else:
            offspring = Individual(genotype=other.genotype)

        return offspring

    def __repr__(self):
        """Representation of an individual"""
        return f"{self.id_code} -> {self.genotype}"


class Population:
    """A class for a population of genetic individuals"""

    def __init__(
        self, n_homozygous_recessive=1, n_homozygous_dominant=1, n_heterozygous=1
    ):
        homozygous_dominant = [
            Individual("homozygous dominant") for i in range(n_homozygous_dominant)
        ]
        homozygous_resessive = [
            Individual("homozygous recessive") for i in range(n_homozygous_recessive)
        ]
        heterozygous = [Individual("heterozygous") for i in range(n_heterozygous)]
        self.individuals = homozygous_resessive + heterozygous + homozygous_dominant
        self.size = len(self.individuals)

    def __repr__(self):
        representation = f"A population of {len(self.individuals)}"
        return representation

    def mating_set(self) -> tuple:
        mating_set = []
        for member_1 in self.individuals:
            for member_2 in self.individuals:
                if member_1 is not member_2:
                    mating_set += [
                        (
                            member_1,
                            member_2,
                        )
                    ]
        return mating_set

    def mating_counts(self) -> dict:
        """Return the counts of each mating genotype"""

        # First get all the combinations.
        combinations = self.mating_set()
        mating_events = [x.mate(y).genotype for x, y in combinations]
        mating_counts = {
            genotype: mating_events.count(genotype) for genotype in mating_events
        }
        return mating_counts

    def mating_probability(self) -> dict:
        """Return all the possible mating chances in the population"""
        # First get all the combinations.
        mating_counts = self.mating_counts()
        total_options = sum([v for v in mating_counts.values()])
        mating_probabilities = {k: v / total_options for k, v in mating_counts.items()}
        return mating_probabilities


if __name__ == "__main__":
    main()
