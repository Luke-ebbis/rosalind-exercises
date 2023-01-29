#!/usr/bin/env python3
'''
TRANSLATION OF A RNA SEQUENCE TO PROTEIN                             sb_prot.py

AUTHOR          Sibbe Bakker
DESCRIPTION         A script to translate a given RNA sequence into protein.
                using the standard genetic code.
INPUTS              Input can be taken from a file or standard in. Must be a
                single line of RNA.
OUTPUT              Output can be send to a file, or to standard out. The
                output is a single line of text containing the protein
                sequence.
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


def give_out(output) -> None:
    fileout = ''
    stout = False  # Indicate whether there is output written to STDout.
    if '-filter' in arguments:
        if len(arguments) == 1:
            fileout = arguments[0]
            sys.stdout.write(output)
            stout = True
        else:
            fileout = arguments[0]

    elif len(arguments) == 1:
        fileout = arguments[0]
        sys.stdout.write(output)
        stout = True
    else:
        fileout = arguments[1]
    if stout == False:
        assert os.path.exists(fileout) == False, \
            f"The output file {fileout!r} cannot be used as it exists."
        with open(fileout, 'w') as out:
            out.write(output)


def translate_rna(s: str) -> str:
    """
    AUTHOR          Sibbe Bakker
    LIMITATION          The translation can only be done using the standard
                    genetic code.
    :param s: str: The RNA sequence to be translated into protein.
    :return: str: The amino acid sequence of the protein encoded by `s`.
    """
    protein = None
    # STANDARD GENETIC CODE AS A DICT:
    code = \
        {  # The standard genetic code.
            'UUU': 'F', 'CUU': 'L', 'AUU': 'I', 'GUU': 'V',
            'UUC': 'F', 'CUC': 'L', 'AUC': 'I', 'GUC': 'V',
            'UUA': 'L', 'CUA': 'L', 'AUA': 'I', 'GUA': 'V',
            'UUG': 'L', 'CUG': 'L', 'AUG': 'M', 'GUG': 'V',
            'UCU': 'S', 'CCU': 'P', 'ACU': 'T', 'GCU': 'A',
            'UCC': 'S', 'CCC': 'P', 'ACC': 'T', 'GCC': 'A',
            'UCA': 'S', 'CCA': 'P', 'ACA': 'T', 'GCA': 'A',
            'UCG': 'S', 'CCG': 'P', 'ACG': 'T', 'GCG': 'A',
            'UAU': 'Y', 'CAU': 'H', 'AAU': 'N', 'GAU': 'D',
            'UAC': 'Y', 'CAC': 'H', 'AAC': 'N', 'GAC': 'D',
            'UAA': '*', 'CAA': 'Q', 'AAA': 'K', 'GAA': 'E',
            'UAG': '*', 'CAG': 'Q', 'AAG': 'K', 'GAG': 'E',
            'UGU': 'C', 'CGU': 'R', 'AGU': 'S', 'GGU': 'G',
            'UGC': 'C', 'CGC': 'R', 'AGC': 'S', 'GGC': 'G',
            'UGA': '*', 'CGA': 'R', 'AGA': 'R', 'GGA': 'G',
            'UGG': 'W', 'CGG': 'R', 'AGG': 'R', 'GGG': 'G'
        }

    # Extract the codons by taking a three base long stretch every 3 bases.
    codons = [s[index:index+3] for index in range(0, len(s.strip('\n')), 3)]
    protein_list = [code[codon] for codon in codons if code[codon] != '*']
    protein = ''.join(protein_list)
    return protein

def main():
    inputstr = take_input()
    sequence = inputstr.upper()
    protein = translate_rna(sequence)
    give_out(f'{protein}\n')


if __name__ == "__main__":
    main()
