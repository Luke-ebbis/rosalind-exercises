#!/usr/bin/env python3
'''
COUNT OF DNA NUCLEOTIDES IN A SEQUENCE                                 SB_DNA.py

AUTHOR          Sibbe Bakker
DESCRIPTION         A script to count the nucleotides in a DNA string, and
                present the sums in A, C, G, T.
INPUTS              The inputs are taken from STDIN and given back out to STDOUT.
                Additionally, a file name is also accepted. If standard in is
                taken as input, the flag `-filter' must be given. First imput
                is the file with the sequence, second input is the out file.
                if this is not present, output is given to STDOUT. Filter must
                be given at the end.

LIMITATION      The input may only contain the 4 bases of DNA.
Useage          The script is used as follows:
   cat rosalind_dna.txt | ./SB_DNA.py -filter
'''

import sys, os

DNA_letters = {'a', "t", "c", 'g'}
arguments = sys.argv[1:]


def take_input() -> str:
    input = ''
    if "--input-file" in arguments:
        with open(arguments[1], "r") as f:
            return "".join([x.strip("\n").lower() for x in f.readlines()])
    else:
        raise Exception("Error with handling the file")


def count_letters(instr: str) -> dict:
    '''
    A FUNCTION TO COUNT THE CHARACTER OCCURRENCE                 count_letters()
    
    :author:            Sibbe Bakker
    :param instr:       The string for which the strings are counted. The
                        counting is done using a comprehension.

    :return: 
    '''

    # make a dictionary of the letters in the string:
    stats = {L: instr.count(L) if L in instr else 0  for L in instr}
    missing_set = {L:0 for L in DNA_letters if L not in stats.keys()}
    return stats | missing_set


def give_out(output) -> None:
    '''
    :param output:      A dictionary containing 4 keys, A, T, C and G
    :return:            Nothing.
    '''
    fileout = ''
    STOUT = False # a flag to indicate whether there is output written to STDout

    letters = {k for k in output.keys()}
    assert len((letters ^ DNA_letters)-DNA_letters) == 0,\
        f"The input sequence contains nonstandard letters {DNA_letters^letters}"

    A = str(output['a'])
    C = str(output['c'])
    G = str(output['g'])
    T = str(output['t'])


    outstr = f"{' '.join([A,C,G,T])}\n"
    print(outstr)

def main():
    instr = take_input()
    stats = count_letters(instr)
    give_out(stats)



if __name__ == "__main__":

    main()
