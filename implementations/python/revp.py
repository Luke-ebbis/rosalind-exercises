#! /usr/bin/env python3
"""A PYTHON SCRIPT TO FIND A REVERSE PALINDROME IN DNA

author --- Sibbe Bakker

usage --- Input must be fasta file. The output will be a tab separated list
of all reverse palindromic pairs in the sequence. One indexed.

examples:
 ./name -filter Take and give to standard io.

 ./name file Take from file, give to stdout.

 ./name file -filter Take from standard in, give to file.

 ./name file1 file2 Take from file1, write to file2.

dependencies --- sys, os, re.

description --- This python file finds the reverse palindromes in a 'single'
FASTA file. A reverse palindrome is a DNA sequence which is equal to its
reverse complement.
"""
import sys, os, re

ARGUMENTS = sys.argv[1:]


def read_fasta(fp) -> tuple:
    """Parsing a fasta file via the yield keyphrase.

    :param fp: _io.TextIOWrapper: An file object of a FASTA file, as created
     using `open(x) as x_alias`.
    :return: tuple: The fasta file in tuple structure
     (name:str, sequence:str).

    description --- Parsing with yield is more memory efficient than with
    return.
    original source --- https://stackoverflow.com/questions/29805642/learning-
    to-parse-a-fasta-file-with-python.
    """

    if type(fp) == str:
        fp = fp.split('\n')

    name, seq = None, []
    for line in fp:
        # remove the non printable characters from the end of the line
        line = line.rstrip()
        if line.startswith(">"):
            # When there is a name already seen.
            if name:
                yield (name, ''.join(seq))
            # Always reset the accumulators: set the name to the
            # current line, and set the sequence to empty.
            name, seq = line, []
        else:
            # when the line does not start with >, accumulate the sequence
            # with lines.
            seq.append(line)
    if name:
        # Since the last record does not end with an >, we need to yield the
        # last record also.
        yield (name, ''.join(seq))


def take_input() -> str:
    """
    TAKING IN DYNAMIC INPUT FROM THE COMMAND LINE

    AUTHOR          Sibbe Bakker
    DESCRIPTION     This
    :return:        A string with the input to the programme.
    """
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
            fileout = ARGUMENTS[0]
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


def make_rev_comp(dna: str) -> str:
    ''' A FUNCTION TO DETERMINE THE REVERSE COMPLEMENT

    :structures:
    :param dna: str: The DNA string.
    :return: str: The reverse complement.

    author --- Sibbe Bakker.
    '''

    # make a dictionary of the letters in the string:
    revdna = reversed(dna)
    revc_list = []
    for s in [s for s in revdna]:
        if s == 'T':
            revc_list += 'A'
        if s == 'A':
            revc_list += 'T'
        if s == 'C':
            revc_list += 'G'
        if s == 'G':
            revc_list += 'C'
    revcdna = ''.join(revc_list)
    return revcdna


def kmers_with_index(x: str, k: int = 3) -> list:
    """Retrieve Kmers from a string

    :param str x: str: The string for which the kmers need to be determined.
    :param: int k: int: The size of the kmer. Defaults to int:3.
    :return: tuple: A tuple containing the kmers with their index. The
     structure is ((kmer1, (start, stop)).

    author --- Sibbe Bakker
    """
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
            index_found = (start, k_current_index,)
            kmers.append((kmer_found, index_found))
            start += 1
            k_current_index += 1
            end = k_current_index
    return kmers


def extend_kmer_with_index(kmer: tuple, sequence: str,
                           direction: str = 'forward') -> tuple:
    """
    Extending a kmer with an index

    :param direction: str: direction to extend the kmer into, either 'forward'
     or 'reverse' direction.
    :param sequence: str: the DNA sequence from which the tuple originates.
    :param kmer: a kmer with the indexing in the form of (kmer:str,
      (start:int, stop:int).
    :return: tuple: the extended kmer with index. Returns an empty tuple if
     the kmer exists in the sequence but cannot be extended in the specified
     direction.
    """
    kmer_seq, index = kmer
    kmer_start, kmer_stop = index
    assert find_exact_query(sequence, kmer_seq), \
        ValueError('The kmer does not exist in the given sequence.')

    extended_kmer = (None,)  # assume that the kmer cannot be extended.
    if direction == 'forward':
        new_stop = kmer_stop + 1
        try:
            new_kmer = sequence[kmer_start: new_stop]
            extended_kmer = (new_kmer, (kmer_start, new_stop))
        except:
            # kmer cannot be extended
            pass
    else:
        new_start = kmer_start - 1
        try:
            new_kmer = sequence[new_start: kmer_stop]
            extended_kmer = (new_kmer, (new_start, kmer_stop))
        except:
            # kmer cannot be extended
            pass
    return extended_kmer


def find_exact_query(s: str, q: str) -> tuple:
    """
    FINDING THE EXACT OCCURRENCES OF A QUERY IN A SUBJECT STRING

    :structures:
    :param str s: Subject, a sequence of DNA.
    :param str q: Query, a sequence of DNA.
    :return: tuple: A list of _starting_ indexes (int) of each occurance
     of string q in s. If no matches are found, the tuple contains a single
     element of `None`.

    AUTHOR---Sibbe Bakker
    DESCRIPTION---To find matches of a substring of a biological string,
    the subject string is broken in to kmers, where k is the length of the
    query string. The resulting list of kmers is then searched for
    occurrences of the query string.
    DEPENDENCIES---kmers_with_index()
    """

    # This regular expression takes care of the codon issues
    query = q

    kmers_list = kmers_with_index(s,
                                  k=len(q))  # The kmers that are in the input

    indexes = []
    for kmer_i in range(len(kmers_list)):
        kmer, index = kmers_list[kmer_i]
        if kmer == query:
            # Appending the start, adding 1 indexing
            indexes.append(index[0] + 1)

    if indexes == []:
        indexes.append(None)

    return tuple(indexes)


def is_reverse_palindrome(dna: str) -> bool:
    """Check if a string is a reverse palindrome.

    :param dna: str:a string of dna.
    :return: bool: whether or not `dna` is a reverse palindrome. `True` for
     yes. `False` for no.
    """
    dna_rev = make_rev_comp(dna)
    return dna_rev == dna


def find_reverse_palindromes(dna: str, min_length: int = 4,
                             max_length: int = 12,
                             verbose=False) -> tuple:
    """
    A function to find the reverse complement palindromes  in a string

    :param dna: str: the string of dna to be searched for reverse palindromes.
    :param min_length: int: minimum length of reverse palindromes to look for.
    :param max_length: int: maximum length of reverse palindromes to look for.
    :param verbose: bool: whether to print status messages. False by default.
    :return: tuple: Palindrome pairs (complement:str,
     (position:int, length:int))

    author --- Sibbe Bakker.\n
    description --- A reverse palindrome is indicative of a restriction site
     in the DNA. The exact definition of a reverse palindrome is a sequence
     that is equal to its reverse complement.  For example: GCATGC is equal
     to the reverse complement: GCATGC.

    """

    assert min_length < max_length, \
        ValueError(f'The minimum reverse palindrome length (currently '
                   f'{min_length}) must be smaller than the maximum reverse'
                   f'palindrome length (currently {max_length})')
    # The iterable in which the reverse palindromes are to be accumulated.
    reverse_palindromes = []

    # To find all palindromes with minimum length, first make kmers of minimum
    # length, then extend these kmers until it reached its maximum length.

    for kmer in kmers_with_index(dna, k=min_length):
        if is_reverse_palindrome(kmer[0]):
            # this finds the shortest one, not all of them
            if verbose: print(f'*: saving {kmer} directly')
            reverse_palindromes.append(kmer)

        # Initial hit came up empty start extending

        # untill [the kmer is longer than query,
        #          or longer than the max search]
        keep_extending = [True, True]
        # Extend 1
        kmer_to_extend = extend_kmer_with_index(kmer, dna)
        while all(keep_extending) == True:
            is_shorter_than_query = len(kmer_to_extend[0]) < len(dna) \
                                    - kmer_to_extend[1][0]  # kmer start
            is_shorter_than_max_search = len(kmer_to_extend[0]) <= \
                                         max_length

            keep_extending = [is_shorter_than_query,
                              is_shorter_than_max_search]

            if all(keep_extending) == True:
                kmer_to_extend = extend_kmer_with_index(kmer_to_extend,
                                                        dna)
            else:
                # this kmer cannot be extended to a reverse palindrome
                if verbose:
                    print(f'X: kmer {kmer_to_extend} cannot be extended:'
                          f'{keep_extending}, {len(kmer_to_extend[0])}')
                break

            if is_reverse_palindrome(kmer_to_extend[0]):
                reverse_palindromes.append(kmer_to_extend)
                if verbose:
                    print(f'*: saving {kmer_to_extend} from'
                          f' {kmer} during while.')
        else:
            reverse_palindromes.append(kmer_to_extend)
            if verbose:
                print(f'*: saving {kmer_to_extend} from {kmer} at'
                      f' the end of while')
    return tuple(reverse_palindromes)


def format_palindrome_report(reverse_palindromes: tuple) -> str:
    """Reporting the reverse palindromes tuple

    :param reverse_palindromes: tuple: a tuple in the form (sequence:str
     (start:int, stop:int)).
    :return: strA tab separated string of the start positions of each reverse
     palindrome and its length, ordered on start position.

    author --- Sibbe Bakker.
    """
    # here x[0][1] refers to the start index.
    sorted_palindromes = sorted(reverse_palindromes, key=lambda x: x[1][0])
    report_lines = []
    for palindrome in sorted_palindromes:
        # + 1 for 1 indexing
        report_lines.append(f'{palindrome[1][0] + 1} '
                            f'{palindrome[1][1] - palindrome[1][0]}')
    return '\n'.join(report_lines) + '\n'


def format_palindrome_report_sep(reverse_palindromes: tuple,
                                 max_length: int = 13,
                                 adjust_ws: bool = False,
                                 sep='\t') -> str:
    """Reporting the reverse palindromes tuple

    :param reverse_palindromes: tuple: a tuple in the form (sequence:str
     (start:int, stop:int)).
     :param sep: str: String to use as the delimiter.
    :max_length: int: maximum length of the palindromes to be reported.
    :return: strA tab separated string of the start positions of each reverse
     palindrome and its length, ordered on start position.

    author --- Sibbe Bakker.
    """
    # here x[0][1] refers to the start index.
    sorted_palindromes = sorted(reverse_palindromes, key=lambda x: x[1][0])

    sequence_align = '<'

    if adjust_ws:
        seq_head = f"{'seq':{sequence_align}{max_length}}"
    else:
        seq_head = "Seq"

    header = ["S", seq_head,
              "L"]
    report_lines = [sep.join(header)]
    for palindrome in sorted_palindromes:
        # + 1 for 1 indexing
        if adjust_ws:
            sequence_data = f'{palindrome[0]:{sequence_align}{max_length}}'
        else:
            sequence_data = f'{palindrome[0]}'

        data_line = [f'{palindrome[1][0] + 1}',
                     sequence_data,
                     f'{palindrome[1][1] - palindrome[1][0]}']

        report_lines.append(sep.join(data_line))
    return '\n'.join(report_lines) + '\n'


def main():
    """The main function

    :return: Nothing
    """
    # Take input from file _or_ stdin as a single string
    input_text = take_input()
    # Parse header, comment and sequence fields
    fastas = read_fasta(input_text)
    fastas_list = []
    for i in fastas:
        fastas_list.append(i)
    dna = fastas_list[0][1]
    # Obtain the reverse palindromes of the input DNA sequence.
    reverse_palindromes = find_reverse_palindromes(dna, verbose=True)
    # Output a formatted table of the reverse palindrome start sites.
    give_out(format_palindrome_report_sep(reverse_palindromes))

    #GCATGC
if __name__ == '__main__':
    main()
