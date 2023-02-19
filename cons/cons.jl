#! /usr/bin/env julia
"""Making a consensus matrix
author --- Sibbe Bakker
"""

function main()
    try
        input = take_file_input()
        data = parse_fasta(input)
        profile =  profile_matrix(data)
        println(typeof(profile))
    catch
        throw("Input file could not be analysed.")
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
        return input
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

function check_equal_sequence_length(list :: Vector{Dna})
    # Test if all sequences are equal length
    first_l = length(list[1].sequence)
    check = map(x -> 
                first_l != length(x.sequence),
                list[2:end])
    if all(check) == true  throw("unequal sequences") end
    #TODO make this error message say the identifier.
    # println(list)
    # if all(check) == false
    #     shorter_sequences = []
    #     for i = 1:length(list) - 1
    #         if check[i] == true
    #             push!(shorter_sequences,
    #                   list[i].identifier)
    #         end
    #     end

    #     throw("Sequence $shorter_sequences too short.")
    # end
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

"""
    profile_matrix(sequences::Vector{Dna})::Matrix

Construct a profile matrix from a vector of Dna sequences.

...
# Arguments
- `sequences::Vector{Dna}::Matrix`: 
...

# Depends

- `check_equal_sequence_length()`

# Example
```julia
```
"""
function profile_matrix(Dna_sequences :: Vector{Dna}) :: Tuple{Vector{Char}, 
                                                               Matrix{Float64}}
    check_equal_sequence_length(Dna_sequences) 
    sequences = map(x -> x.sequence, Dna_sequences)
    sequenceₗ = length(sequences[1])
    row_names= [] 
    
    # Letters are the row names.
    letters = collect(keys(enumerate_characters(join(sequences))))
    # Pre-allocate all the values of the array.
    profile = zeros(length(letters), sequenceₗ)
    
    for index = 1:sequenceₗ
        for sequence_index = 1:length(sequences)
            for index_character = 1:length(letters)
                current_char = sequences[sequence_index][index]
                println(current_char, string(letters[index_character]))
                if string(current_char) == string(letters[index_character])
                    profile[index_character, index] += 1
                    println("seq $sequence_index char $index  $current_char")
                end
            end
        end

    end
    return (letters, profile)
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






