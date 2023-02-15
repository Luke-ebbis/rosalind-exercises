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

function validate_dna(string::String)
    #=Validating DNA input

    :param string: String The string to be validated. 
    :return: Bool: 1 If the string is a valid DNA string, 0 otherwise.
    =#
    accepted_letters = Set(['A', 'T', 'C', 'G'])
    accepted_chars = map(x -> issubset(x, accepted_letters), collect(string))
    return all(accepted_chars)
end

function enumerate_characters(string::String)
    #=Counting the number of unqiue characters in a given string
    =#
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
