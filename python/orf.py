#! /usr/bin/env python3
"""2022-11-08             PYTHON SCRIPT                                  orf.py
FINDING THE TRANSLATED OPEN READING FRAMES IN A FASTA SEQUENCE

USAGE               Input must be a single FASTA record (from the header and
                one sequence). Output are the proteins that can be obtained
                by transcribing and translating the FASTA record.
                ./name -filter          Take and give to standard io.
                ./name file             Take from file, give to stdout.
                ./name file -filter     Take from standard in, give to file.
                ./name file1 file2      Take from file1, write to file2.
DEPENDENCIES    sys, os, re.
"""
import sys, os, re

ARGUMENTS = sys.argv[1:]

#   export PYTHONPATH=/home/sibbe/Documents/study/Msc-at-WUR/parts/courses/P2-mo-advanced_bioinformatics-BIF30806/practical_work/week_1/p2/
#os.environ["PYTHONPATH"] ="bla"

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
    codons = make_codons(s)
    protein_list = [code[codon] for codon in codons if code[codon] != '*']
    protein = ''.join(protein_list)
    return protein

def take_input() -> str:
    '''
    TAKING IN DYNAMIC INPUT FROM THE COMMAND LINE

    AUTHOR          Sibbe Bakker
    DESCRIPTION     This
    :return:        A string with the input to the programme.
    '''
    global ARGUMENTS
    input = ''
    if '-filter' in ARGUMENTS:
        # Take from STDIN; ignore files that are given.
        input = sys.stdin.read()
        assert os.path.exists(ARGUMENTS[0]) == False, \
            f"The input file {ARGUMENTS[0]!r} cannot be used as it exists."
        # todo how to break this over lines?
    else:
        infile = ARGUMENTS[0]
        assert os.path.exists(infile), \
            FileExistsError(f"The supplied file {infile!r} does not exist.")
        with open(infile) as f:
            input = f.read()

    assert input != '', ValueError("Input could not be taken.")
    return input


def give_out(output: str) -> None:
    """2022-11-06              PYTHON FUNCTION                       give_out()
    A FUNCTION FOR DYNAMIC OUTPUT

    DESCRIPTION         This function can take input dynamically, either from
                    standard input, or a file.
    GLOBALS             This function requires the declaration of the global
                    ARGUMENTS variable. This variable is a list of the
                    arguments passed to the python script.
    DEPENDENCIES    sys, os.
    """

    global ARGUMENTS
    fileout = ''
    stout = False  # a flag to indicate whether there is output written to STO
    if '-filter' in ARGUMENTS:
        if len(ARGUMENTS) == 1:
            fileout = ARnameGUMENTS[0]
            sys.stdout.write(output)
            stout = True
        else:
            fileout = ARGUMENTS[0]

    elif len(ARGUMENTS) == 1:
        fileout = ARGUMENTS[0]
        sys.stdout.write(output)
        stout = True
    else:
        fileout = ARGUMENTS[1]
    if stout == False:
        assert os.path.exists(fileout) == False, \
            f"The output file {fileout!r} cannot be used as it exists."
        with open(fileout, 'w') as out:
            out.write(output)

def parse_mono_fasta(fasta_record:str) -> tuple:
    """
    PARSING A SINGLE FASTA RECORD

    :param fasta_record: A _single_ fasta record to be parsed.
    :return: tuple: (header:str, sequence:str)
    """
    dna_buffer = []
    for line in fasta_record.split('\n'):
        if line.startswith('>'):
            key = line.replace('>', '').strip()
        else:
            dna_buffer += line.strip('\n')

    dna = ''.join(dna_buffer)
    out = (key, dna,)
    return out


def make_rev_comp(DNA: str) -> str:
    '''
    A FUNCTION TO DETERMINE THE REVERSE COMPLEMENT              make_rev_comp()

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

def make_codons(s:str)->tuple:
    """
    MAKE THE CODONS OF A SEQUENCE.

    :param s: str: A sequence that must be converted into triplets.
    :return:
    """
    out = tuple([s[index:index + 3] for \
                 index in range(0, len(s.strip('\n')), 3)])
    return out


def kmers_with_index(x: str, k: int=3) -> list:
    '''
    Retrieve Kmers from a string
    AUTHOR  Sibbe Bakker


    :rtype: object
    :param x: str: The string for which the kmers need to be determined.
    :param: k: int: The size of the kmer. Defaults to int:3.
    :return: tuple: A tuple containing the kmers with their index. The
    structure is ((kmer1, (start, stop)).
    :except: ValueError: When the selected value of `k`
    '''
    assert k <= len(x), \
        ValueError(f"The value of k is {k}, it must not be "
                   f"bigger than the length of x ({len(x)}) \n x is {x}.")
    kmers = []
    start = 0
    end = 0
    k_current_index = k
    if k <= len(x):
        while end <= len(x):
            kmer_found = x[start:k_current_index]
            index_found = (start, k_current_index, )
            kmers.append((kmer_found, index_found))
            start += 1
            k_current_index += 1
            end = k_current_index
    return kmers

def find_orfs(s:str) -> list:
    """
    A FUNCTION TO FIND OPEN READING FRAMES IN A STRING
    :param s: str: A sequence of DNA.
    :return: The list of orfs each element being a string.
    """

    # This regular expression takes care of the codon issues
    # Gebruik start+1.
    orf = re.compile(r"((ATG|AUG)(.{3})*?(TAA|UAA|TAG|UAG|TGA|UGA))")


    rev_s = make_rev_comp(s)

    forward_orfs = orf.finditer(s)
    reverse_orfs = orf.finditer(rev_s)
    orfs_list = [forward_orfs, reverse_orfs]
    frames_list = []
    for direction in orfs_list:
        for match_obj in direction:
            frames_list.append(match_obj.group(1))
    return frames_list


def find_orfs_no_regex(s:str) -> tuple:
    """
    FINDING THE OPEN READING FRAMES IN A DNA STRING (no regex, no issues!)

    AUTHOR          Sibbe Bakker
    DESCRIPTION         To find the open reading frames in a biological
                    string, there are three stages. The first stage consist of
                    finding the kmers that represent a start codon. The second
                    stage is putting all triplets between the start kmer and
                    the stop codon. The last stage consist of restarting the
                    algorithm at the last start kmer.

    :param s: str: A sequence of DNA.
    :return: tuple: The tuple of orfs each element being a string.
    """

    # This regular expression takes care of the codon issues
    start = set(['ATG'])
    stop = set(['TGA', 'TAG', 'TAA'])

    assert len(s) % 3 == 0, ValueError("The provided string does"
                                       "not have complete "
                                       "triplets.")

    frames_list = [] # The list of orfs that will be accumulated to.

    kmers_list = kmers_with_index(s, k=3) # The 3mers that are in the input str:s.
    last_start = 0
    write_buffer = False
    for kmer_i in range(len(kmers_list)):
        # Stage 1: finding the start.
        kmer, index = kmers_list[kmer_i]

        if kmer in start:
            # Stage 2: Start has been found, accumulating an orf.
            if not kmer_i == last_start:
                # This kmer has not been seen.
                last_start = kmer_i
                write_buffer = True
            else:
                # This kmer has been seen, we skip to the next kmer.
                write_buffer = False

            # Find the codons from start possition to the end of string
            codons = make_codons(s[index[0]:])

            # Mow extend the triplets untill a stop.
            orf_buffer = []
            for codon in codons:
                # Accumulate the codons
                orf_buffer.append(codon)

                if codon in stop:
                    # If the codon is stop, dump buffer into store and reset.
                    frames_list.append(''.join(orf_buffer))
                    orf_buffer = []
                    break

    return frames_list



def main():
    """
    THE MAIN FUNCTION
    :return: Nothing
    """
    input_text = take_input()
    parsed_fasta = parse_mono_fasta(input_text)
    dna = parsed_fasta[1].upper()
    orfs_forward = find_orfs(dna)
    orfs_reverse = find_orfs(make_rev_comp(dna))
    print(make_rev_comp(dna))
    orfs = [orfs_forward, orfs_reverse]
    orfs_flat =  []
    for orf in orfs:
        for record in orf:
            orfs_flat.append(record)

    mras = [orf.replace('T', "U") for orf in orfs_flat]

    # Make the proteins in a set comprehension to fetch the distinct ones.
    protein = {translate_rna(rna) for rna in mras}
    give_out('\n'.join(protein) + '\n')

if __name__ == '__main__':
    main()
