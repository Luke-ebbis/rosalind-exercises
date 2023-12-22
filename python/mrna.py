#!/usr/bin/env python3
"""Inferring mRNA from protein

author --- Sibbe Bakker

description --- Inferring the mRNA from a protein sequence.
"""
import re, subprocess, os, sys, argparse

# a global verbosity variable.
VERBOSE = False


def vprint(x: any, v: bool = False) -> None:
    """A wrapper around print that can be turned off

    :param x: What needs to be printed to the terminal using the python
        function print.
    :param v: bool: whether the print should be turned on. When True, x is
        printed. Default is False.
    :return: None: It is a procedure.

    author --- Sibbe Bakker.
    """
    if v:
        try:
            print(x)
        except:
            print(f'variable x not defined.')

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
    pass

if __name__ == "__main__":
    main()