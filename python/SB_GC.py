#!/usr/bin/env python3
'''
DETERMINING THE MAX GC CONTENT                                            rna.py

AUTHOR          Sibbe Bakker
DESCRIPTION     A script to determine the maximum gc content of a fasta file.
INPUTS              Input can be taken from a file or standard in. Must be DNA
                in a fasta format.
OUTPUT              Output can be send to a file, or to standard out. The output
                is the fasta header of the sequence with the highest GC sequence
                with the GC percentage as a float below.
USAGE           ./name -filter          Take and give to standard io
                ./name file             Take from file, give to stout
                ./name file -filter     Take from standard in, give to file
                ./name file1 file2      Take from file1, write to file two
'''

import sys, os

arguments = sys.argv[1:]


def take_input() -> str:
    """take_input.

    :rtype: str
    """
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


def give_out(output) -> None:
    fileout = ''
    stoud = False  # a flag to indicate whether there is output written to STDout
    if '-filter' in arguments:
        if len(arguments) == 1:
            fileout = arguments[0]
            sys.stdout.write(output)
            stoud = True
        else:
            fileout = arguments[0]

    elif len(arguments) == 1:
        fileout = arguments[0]
        sys.stdout.write(output)
        stoud = True
    else:
        fileout = arguments[1]
    if stoud == False:
        assert os.path.exists(fileout) == False, \
            f"The output file {fileout!r} cannot be used as it exists."
        with open(fileout, 'w') as out:
            out.write(output)

def fasta_to_dict(fasta:str) -> dict:
    lines = fasta.split('\n')
    lines_dict = {}

    # look at each line of input
    for lineN in range(len(lines)):
        if lines[lineN].startswith('>'):
            # make the key the header of the fasta.
            key = lines[lineN].strip('>')

            # If the current line is a header, next one will be a sequence.
            SeqLineN = lineN + 1
            lines_dict[key] = [lines[SeqLineN]]

            # todo this part should be rewritten into something I understand.
            # python should stop reading if it encounters a header, or a nothing
            while not lines[SeqLineN].startswith('>') and lines[SeqLineN] != '':
                if lines[SeqLineN] != '':
                    SeqLineN += 1
                    if not lines[SeqLineN].startswith('>'):
                        lines_dict[key] += list(lines[SeqLineN])
                else: # stop reading if the checks do no pass
                    break

    fasta_dict = {k:''.join(v) for k,v in lines_dict.items()}
    return fasta_dict


def count_letters(instr: str) -> dict:
    '''
    A FUNCTION TO COUNT THE CHARACTER OCCURRENCE                 count_letters()

    :author:            Sibbe Bakker
    :param instr:       The string for which the strings are counted. The
                        counting is done using a comprehension.

    :return:
    '''

    # make a dictionary of the letters in the string and fill it with the counts:
    stats = {L: instr.count(L) for L in instr}
    return stats

def compute_gc(DNA: dict) -> dict:
    # first count the occurance of each letter.
    stats = {k:count_letters(v) for k,v in DNA.items()}
    # next determine the percentage of G/C over length of the sequence.
    GC = ({k: ((v['C']+v['G']) / sum(v.values())) * 100 \
           for k, v in stats.items()})
    return GC

def main():
    fasta_file = take_input()
    fasta_dict = fasta_to_dict(fasta_file)
    fasta_dict_stats = compute_gc(fasta_dict)

    # select the max here
    max_pair = ('', 0)
    for k, v in fasta_dict_stats.items():
        if v > max_pair[1]:
            max_pair = (k ,v)

    # Making the output look oke; using list() is quicker here
    outstr = f"{max_pair[0]}\n" \
             f"{max_pair[1]:9f}\n"

    give_out(outstr)

if __name__ == "__main__":
    main()

