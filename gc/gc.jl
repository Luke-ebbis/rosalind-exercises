#! /usr/bin/env julia
#=Determining the CG content of a DNA string
=#



"""
    main()

This is the main procedure.
"""
function main()

    # if length(ARGS) != 2
    #     println("usage: $PROGRAM_FILE  <data> \n"*
    #             "data \t The number of generations." )
    #     exit(1)
    # end

    # input_strings = ARGS
    # n, k = map((x) -> parse(Int, x), input_strings)

    #TODO make the string uppercase
    input = open("./test.fasta", "r")
    data = parse_fasta(input)
    println(data)
    exit(0)
end


"""
    parse_fasta(inputObject::IOStream)

Parse a fasta file, return a list of Dna data types.

...
# Arguments
- `inputObject::IOStream`: An open fasta file object.
...

# Example
```julia
```


"""
function parse_fasta(inputObject :: IOStream)
    fastas = Dict()

    sequence, identifier, comment = String[], "", ""
    for line in eachline(inputObject)
        clean_line = strip(line)

        if startswith(clean_line, ">")
            header = replace(clean_line, ">" => "")
            if contains(clean_line, " ")
                (identifier, comment) = split(header, limit=2)
            else
                identifier = header
                comment = ""
            end
            sequence = String[]
            fastas[identifier] = ([], comment)
        else
            push!(fastas[identifier][1], clean_line)
        end
    end
    
    dnas = []
    for item in fastas
        name = item[1]
        comment = item[2][2]
        sequence = join(item[2][1])
        push!(dnas,
              Dna(sequence,
                  String(name),
                  String(comment))
             )
    end
    return dnas
end

"""A datatype for DNA

:param sequence: String: The sequence of the DNA.
:param: identifier: String: identifier of the DNA.
:param:  comment: The comment of the DNA string.
"""
struct Dna
    sequence::AbstractString
    identifier::AbstractString
    comment::AbstractString
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
:return: Dict{Char, Int}: A dictionary where each letter is a key and the value is the amount of occurrences of that letter.
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
