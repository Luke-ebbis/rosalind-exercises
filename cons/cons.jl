#! /usr/bin/env julia
"""Making a consensus matrix
author --- Sibbe Bakker
"""

function main()
    try
        input = take_file_input()
        data = parse_fasta(input)

        println(data)
    catch
        throw("Input file could not be read")
    end
end



# Functions -----------------------------------------------------------------

"""
    take_file_input()

Take in a file name from the command line, and read it into a io file.
"""
function take_file_input()
     if length(ARGS) != 1
        println("usage: $PROGRAM_FILE  <fasta> \n"*
                "fasta \t File path to a fasta file." )
        exit(1)
    end

    arg  = join(ARGS)
    if isfile(arg)
        input = open(arg, "r")
    else
        throw("provided $arg is not a valid file.") 
    end
end

"""
    parse_fasta(inputObject::IOStream)
Parse a fasta file, return a list of Dna data types.
...
# Arguments
- `inputObject::IOStream`: An open fasta file object. As created with the
`open()` function.
...
"""
function parse_fasta(inputObject :: IOStream) :: Vector{Dna}
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
:param sequence: String: The sequence of the DNA. Lowercase letters are
automatically converted to uppercase letters.
:param: identifier: String: identifier of the DNA.
:param:  comment: The comment of the DNA string.
depends --- validate_dna()
"""
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


function profile_matrix(sequences :: Vector{Dna}) :: Matrix
    # Test if all sequences are equal length



    return Matrix{Float64}(undef, 2, 3)
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





# Execute --------------------------------------------------------------------

if abspath(PROGRAM_FILE) == @__FILE__
    # only executing the main function as a script
    main()
end






