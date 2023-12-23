#! /usr/bin/env julia
#="Counting the  number of A, C, G and T in a string of DNA

author --- Sibbe Bakker
description --- A script to count the nucleotides in a DNA string and present
    the number of A, C, G and T as text to standard out.
limitation --- The input string may only contain the set of 4 standard DNA
    bases, these being {A, T, G, C}.
last version --- 2023-01-19
dependencies --- Argparse
=#
function main()
    parsed_args = parse_commandline()
    println("Parsed args:")
    for arg in parsed_args
        println("  $arg  ")
    end
end

function parse_commandline()
    if length(ARGS) != 1
        throw(DomainError(ARGS, "You may only use one argument to this script."))
    else
        return pop!(ARGS)
    end
end

#==A datatype for DNA

:param sequence: String: The sequence of the DNA. Lowercase letters are
automatically converted to uppercase letters.
:param: identifier: String: identifier of the DNA.
:param:  comment: The comment of the DNA string.

depends --- validate_dna()
==#
struct Dna
    sequence::AbstractString
    identifier::AbstractString
    comment::AbstractString
    # Checking Wether the DNA is valid.
    Dna(sequence::String, identifier::String, comment::String) =
    validate_dna(uppercase(sequence)) ? 
        new(uppercase(sequence), identifier, comment) :
        throw(ArgumentError("Sequence of $identifier is invalid."))
end


if abspath(PROGRAM_FILE) == @__FILE__
    main()
    dna_string = read!(AbstractString(parse_commandline()))
    dna = Dna(dna_string, "", "")
    println("Data is $dna")
end
