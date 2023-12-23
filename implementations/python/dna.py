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

arguments = sys.argv[1:]


def take_input() -> str:
    input = ''
    if '-filter' in arguments:
        # Take from STDIN; ignore files that are given.
        input = sys.stdin.read()
        assert not os.path.exists(arguments[0]), \
            f"The input file {arguments[0]!r} cannot be used as it exists."
    else:
        infile=arguments[0]
        assert os.path.exists(infile), \
            FileExistsError(f"The supplied file {infile!r} does not exist.")
        with open(infile) as f:
            input = f.read()
    assert input != '', ValueError("DNA sequence could not be read in/")

    #sanitising input
    input_out = input.strip().lower()

    return input_out

def count_letters(instr: str) -> dict:
    '''
    A FUNCTION TO COUNT THE CHARACTER OCCURRENCE                 count_letters()
    
    :author:            Sibbe Bakker
    :param instr:       The string for which the strings are counted. The
                        counting is done using a comprehension.

    :return: 
    '''

    # make a dictionary of the letters in the string:
    stats = {L:instr.count(L) for L in instr}
    return stats


def give_out(output) -> None:
    '''
    :param output:      A dictionary containing 4 keys, A, T, C and G
    :return:            Nothing.
    '''
    fileout = ''
    STOUT = False # a flag to indicate whether there is output written to STDout

    letters = {k for k in output.keys()}
    DNA_letters = {'a', "t", "c", 'g'}
    assert len(letters ^ DNA_letters) == 0,\
        f"The input sequence contains nonstandard letters {DNA_letters^letters}"

    A = str(output['a'])
    C = str(output['c'])
    G = str(output['g'])
    T = str(output['t'])


    outstr = f"{' '.join([A,C,G,T])}\n"
    if '-filter' in arguments and len(arguments) == 2:
        fileout = arguments[0]
    elif '-filter' in arguments and len(arguments) == 1:
        sys.stdout.write(outstr)
        STOUT = True
    else:
        fileout = arguments[1]
    assert os.path.exists(fileout) == False, \
        f"The output file {arguments[0]!r} cannot be used as it exists."
    if STOUT == False:
        with open(fileout, 'w') as out:
            out.write(outstr)
def main():
    instr = take_input()
    stats = count_letters(instr)
    give_out(stats)



if __name__ == "__main__":

    main()
