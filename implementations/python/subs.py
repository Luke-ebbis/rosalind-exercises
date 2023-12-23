#! /usr/bin/env python3
"""
A PYTHON SCRIPT TO FIND A MOTIF IN DNA
AUTHOR          Sibbe Bakker
USAGE               Input must be two lines of DNA, the first line is the
                sequence that must be searched, the first line has the
                motif that must be found in the first line. The script will
                output the _starting_ positions of all substring occurrences,
                one indexed.
                ./name -filter          Take and give to standard io.
                ./name file             Take from file, give to stdout.
                ./name file -filter     Take from standard in, give to file.
                ./name file1 file2      Take from file1, write to file2.
EXAMPLE SESSION $echo "GATATATGCATATACTT \n ATAT" | ./name -filter
                2 4 10
DEPENDENCIES    sys, os.
"""
import sys, os

ARGUMENTS = sys.argv[1:]


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


def kmers_with_index(x: str, k: int = 3) -> list:
    """
    Retrieve Kmers from a string

    AUTHOR  Sibbe Bakker
    :param str x: str: The string for which the kmers need to be determined.
    :param: int k: int: The size of the kmer. Defaults to int:3.
    :return: tuple: A tuple containing the kmers with their index. The
    structure is ((kmer1, (start, stop)).
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


def find_exact_query(s: str, q: str) -> tuple:
    """
    FINDING THE EXACT OCCURRENCES OF A QUERY IN A SUBJECT STRING

    :param str s: Subject, a sequence of DNA.
    :param str q: Query, a sequence of DNA.
    :return: tuple: A list of _starting_ indexes (int) of each occurrence of
    string q in s. If no matches are found, the tuple contains a single
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


def main():
    """
    THE MAIN FUNCTION
    :return: Nothing
    """
    # input text is a list of lines.
    input_text = take_input().split('\n')
    subject = input_text[0]
    query = input_text[1]
    # print(f'{subject} \n{query}')
    indexes = find_exact_query(subject, query)

    # converting to a printable string
    out = ' '.join([str(index) for index in indexes])
    give_out(out)


if __name__ == '__main__':

    main()
