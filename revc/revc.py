#!/usr/bin/env python3
'''
TRANSCRIBING DNA TO RNA                                                   rna.py

AUTHOR          Sibbe Bakker
DESCRIPTION         A script to calculate the revese complement of a given DNA
                sequence.
INPUTS          Input can be taken from a file or standard in. Must be DNA.
OUTPUT          Output can be send to a file, or to standard out.

USAGE           ./name -filter          Take and give to standard io
                ./name file             Take from file, give to stout
                ./name file -filter     Take from standard in, give to file
                ./name file1 file2      Take from file1, write to file two
'''

import sys, os

arguments = sys.argv[1:]


def take_input() -> str:
    input = ''
    if '-filter' in arguments:
        # Take from STDIN; ignore files that are given.
        input = sys.stdin.read()
        assert os.path.exists(arguments[0]) == False, \
            f"The input file {arguments[0]!r} cannot be used as it exists."
    else:
        infile = arguments[0]
        assert os.path.exists(infile), \
            FileExistsError(f"The supplied file {infile!r} does not exist.")
        with open(infile) as f:
            input = f.read()

    assert input != '', ValueError("Input could not be taken.")

    return input


def make_rev_comp(DNA: str) -> str:
    '''
    A FUNCTION TO DETERMINE THE REVERSE COMPLEMENT               count_letters()

    :author:            Sibbe Bakker
    :structures:
    :param instr:       The DNA string.
    :return:            The reverse complement.
    '''

    # make a dictionary of the letters in the string:
    revDNA = reversed(DNA)
    revc_list = []
    for s in [s for s in revDNA]:
        if s == 'T':
            revc_list+= 'A'
        if s == 'A':
            revc_list+= 'T'
        if s == 'C':
            revc_list+= 'G'
        if s == 'G':
            revc_list+= 'C'
    revcDNA = ''.join(revc_list)
    return revcDNA


def give_out(output) -> None:
    '''
    :param output:      A dictionary containing 4 keys, A, T, C and G
    :return:            Nothing.
    '''
    fileout = ''
    STOUT = False  # a flag to indicate whether there is output written to STDout
    if '-filter' in arguments:
        if len(arguments) == 1:
            fileout = arguments[0]
            sys.stdout.write(output)
            STOUT = True
        else:
            fileout = arguments[0]

    elif len(arguments) == 1:
        fileout = arguments[0]
        sys.stdout.write(output)
        STOUT = True
    else:
        fileout = arguments[1]
    if STOUT == False:
        assert os.path.exists(fileout) == False, \
            f"The output file {fileout!r} cannot be used as it exists."
        with open(fileout, 'w') as out:
            out.write(output)


def main():
    instr = take_input()
    instr = instr.upper().strip()
    DNA = make_rev_comp(instr)
    outstr = f'{DNA.upper()}\n'
    give_out(outstr)


if __name__ == "__main__":
    main()

