#!/usr/bin/env python3
"""Finding a Protein Motif

author --- Sibbe Bakker

description --- This script finds the occurrences of a given PROSITE motif in
the uniprot records specified.
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


def parse_id_file(filepath: str) -> tuple:
    """Parse a id file.

    :param filepath: str: The path to the file where the uniprot id's are
        stored. May contain empty lines, these will be skipped.
    :return: tuple: A tuple of uniprot ids. Each item in the tuple is a
        string.

    dependencies --- os.
    """
    assert os.path.exists(filepath), \
        FileNotFoundError(f'The file {filepath} does not exist.')
    vprint(f'Parsing {filepath}.')
    id_list = []
    with open(filepath) as f:
        for line in f:
            line = line.strip('\n')
            # if the line is not empty.
            if line:
                id_list.append(line)
    return tuple(id_list)


def read_fasta(fp) -> tuple:
    """Parsing a fasta file via the yield keyphrase.

    :param fp: _io.TextIOWrapper|str: An file object of a FASTA file, as
        created using `open(x) as x_alias` or a single string containing
        new line characters.
    :return: tuple: The fasta file in tuple structure
     (name:str, comment:str, sequence:str).

    description --- Parsing with yield is more memory efficient than with
    return.
    original source --- https://stackoverflow.com/questions/29805642/learning-
    to-parse-a-fasta-file-with-python.
    """

    if type(fp) == str:
        fp = fp.split('\n')

    name, comment, seq = None, None, []
    for line in fp:
        # remove the non printable characters from the end of the line
        line = line.rstrip()
        if line.startswith(">"):
            # When there is a name already seen.
            if name:
                yield (name, comment, ''.join(seq))
            # Always reset the accumulators: set the name to the
            # current line, and set the sequence to empty.
            header = line.replace('>', '').split(' ')
            vprint(header, VERBOSE)
            name, comment, seq = header[0], ' '.join(header[1:]), []
        else:
            # when the line does not start with >, accumulate the sequence
            # with lines.
            seq.append(line)
    if name:
        # Since the last record does not end with an >, we need to yield the
        # last record also.
        yield (name, comment, ''.join(seq))


def extract_uniprot_id(s: str, wide_mode:bool=False) -> any:
    """Extracting an uniprot id out of a string.

    :param s: A string containing a uniprot id, such as A2Z669.
    :param wide_mode: bool: Whether to see the whole header as an id. When set
        to True a header such as SP|A2Z669|INTER will be turned into
        SP|A2Z669|INTER. When wide_mode is set to False, this string will be
        turned into A2Z669. False by default.
    :return: any: the uniprot id. If no id can be extracted, None is returned.
        when s has multiple uniprot ids, a tuple will be returned with these
        ids in order of occurrence.
    """
    uniprot_id = None
    if wide_mode:
        uniprot_id_re = re.compile(r'.+')
    else:
        uniprot_id_re = re.compile(r'[A-Z 0-9]{6}')
    finding = uniprot_id_re.findall(s)
    if finding:
        uniprot_id = tuple(finding)
    return uniprot_id


def query_uniprot_fasta(uniprot_id:str, replace_id:bool=True) -> any:
    """Querying the UNIPROT database with curl.

    :param uniprot_id: str: an uniprot id as a single string.
    :param replace_id: bool: Whether to use the uniprot id as the new FASTA
        identifier. The comment field of the FASTA file will stay the same.
    :return: any: If the query was successful, a fasta file is returned as a
        single string. If the query is unsuccessful, None is returned.

    dependencies --- extract_uniprot_id, subprocess.
    author --- Sibbe Bakker
    """
    global VERBOSE
    id = extract_uniprot_id(uniprot_id)[0]
    assert id != None, ValueError(f'The requested id: {uniprot_id} does'
                                  f'not seem to be a valid uniprot id.')
    query = f'https://rest.uniprot.org/uniprotkb/{id}.fasta'
    # the option -q0 sets output to standard out.

    if VERBOSE:
        command = f'wget -O -  {query}'
    else:
        command = f'wget -qO-  {query}'
    vprint(command, VERBOSE)
    result = subprocess.run(command,
                            shell=True,
                            check=True,
                            stdout=subprocess.PIPE)
    fasta = result.stdout.decode()
    if replace_id:
        fasta = re.sub(">([a-z A-Z 0-9 \| \_]+)\s(.+)",
                       f'>{uniprot_id} ' + r'\2', fasta)
    return fasta


def make_regexp_pattern(pattern: str) -> str:
    """Converting a prosite pattern to a regex pattern.

    :param pattern: str: a prosite pattern.
    :return: str: a regular expression pattern.
    """
    global VERBOSE

    # A pattern to detect not groups, like not Pro: {P}.
    not_group = re.compile(r'(\{)(\w+)(\})')
    # A pattern to detect or groups like Cys or Pro: [CP].
    or_group = re.compile(r'(\[)(\w+)(\])')
    # A pattern to detect qualifiers. Match the second digit if present
    qualifier = re.compile(r'(.+)(\()(\d*)[.,]?(\d*)(\))')

    groups = []  # the accumulator
    vprint(f'converting {pattern} to a regex pattern.', VERBOSE)
    for group in pattern.split('-'):
        group_re = group.replace('x', '.')
        if not_group.match(group):
            group_re = not_group.sub(r'[^\2 .]', group_re)
        if or_group.match(group):
            group_re = group_re
        if qualifier.match(group):
            group_re = qualifier.sub(r'\1{\3,\4}', group_re)
            group_re = group_re.replace(',}', '}')
        groups.append(group_re)
        vprint(f'{group} converted to {group_re}', VERBOSE)
    regex_prosite = ''.join(groups)
    regex_lookahead = f"(?=({regex_prosite}))"
    vprint(f'completed pattern {regex_lookahead}.', VERBOSE)
    return regex_lookahead


def find_motif(sequence: str, pattern: str) -> tuple:
    """Find a motif in a sequence.

    :param sequence: str: A string.
    :param pattern: str: A pattern (prosite syntax) for which the motifs need
        to be found.
    :return: tuple:A tuple of motifs start points. Each item in the tuple is
        a tuple with the first item being the name of the sequence as a
        string, and the second item being a list of start positions, each
        item being an integer. The given starting locations are 1 indexed.
    """
    pattern = re.compile(pattern)
    motif_hits = pattern.finditer(sequence)
    starts = []
    for hit in motif_hits:
        starts.append(hit.start() + 1)
    return tuple(starts)


def report_starts(motifs: list) -> str:
    """Reporting the motif locations to a text file.

    :param motifs: A tuple of motifs start points. Each item in the tuple is
        a tuple with the first item being the name of the sequence as a
        string, and the second item being a list of start positions, each
        item being an interger.
    :return: A string of the report. With the name of the sequence and the
        start location of each motif.
    """
    report_lines = []
    for motif in motifs:
        if motif[1]:
            report_lines.append(motif[0])
            # list comprehension to convert the numeric to character
            start_locations_motifs = [str(char) for char in motif[1]]
            starts = ' '.join(start_locations_motifs)
            report_lines.append(starts)
    return '\n'.join(report_lines)

def main() -> None:
    """The main function

    :return:None: Output is to file or standard out.
    """
    file = sys.argv[1]
    motif = 'N-{P}-[ST]-{P}'  # sys.argv[2]
    uniprot_ids = parse_id_file(file)

    # Getting the data from the web and parsing it.
    fasta_list = [query_uniprot_fasta(id) for id in uniprot_ids]
    vprint(fasta_list, VERBOSE)
    fastas = [[result for result in read_fasta(fa)][0] for fa in fasta_list]
    vprint(fastas, VERBOSE)
    motif_re = make_regexp_pattern(motif)

    motif_starts = [(extract_uniprot_id(fa[0], True)[0],
                     find_motif(fa[2], motif_re)) for fa in fastas]

    vprint(motif_starts, VERBOSE)

    print(report_starts(motif_starts))

if __name__ == '__main__':
    main()
