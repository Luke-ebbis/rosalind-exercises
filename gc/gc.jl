#! /usr/bin/env julia
#=Determining the CG content of a DNA string
=#

function main()
    #= The main procedure
    :return: To standard out.
    =#

    # if length(ARGS) != 2
    #     println("usage: $PROGRAM_FILE  <data> \n"*
    #             "data \t The number of generations." )
    #     exit(1)
    # end

    # input_strings = ARGS
    # n, k = map((x) -> parse(Int, x), input_strings)

    #TODO make the string uppercase
    enumerate_characters("AASSSCC")
    exit(0)
end

"""A datatype for DNA

:param sequence: String: The sequence of the DNA.
:param: identifier: String: identifier of the DNA.
:param:  comment: 
"""
struct Dna
    sequence::String
    identifier::String
    comment::String
    # Checking Wether the DNA is valid.
    Dna(sequence::String, identifier::String, comment::String) =
        validate_dna(sequence) ? new(sequence, identifier, comment) :
        throw(ArgumentError("Sequence of $identifier is invalid."))
end

# utilities
"""Validating DNA input

    :param string: String The string to be validated. 
    :return: Bool: 1 If the string is a valid DNA string, 0 otherwise.
"""
function validate_dna(string::String)
    accepted_letters = Set(['A', 'T', 'C', 'G'])
    accepted_chars = map(x -> issubset(x, accepted_letters), collect(string))
    return all(accepted_chars)
end

"""Counting the characters in a string

:param string: String: A string to be analysed.
:return: Dict{Char, Int}: A dictionary where each letter is a key and 
"""
function enumerate_characters(string::String)
    result = Dict{Char, Int}()
    for char in string
        # In case the letter has been found; add to the tally
        if haskey(result, char)
            result[char] += 1
        else
            # initiate the tally
            result[char] = 1
        end
    end
    return result
end

if abspath(PROGRAM_FILE) == @__FILE__
    # only executing the main function as a script
    main()
end
