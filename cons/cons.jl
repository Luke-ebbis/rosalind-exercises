#! /usr/bin/env julia
#=Making a consensus matrix
author --- Sibbe Bakker
=#
using ArgParse
#TODO make a data type for a profile matrix. Try to implement get concensus
# string as a method.

function main()
    s = ArgParseSettings("Construct a consensus string from a fasta file "*
                         "of DNA sequences.")
    try
        @add_arg_table s begin
            "fasta"
            required = true
        end
        parsed_args = parse_args(s)
        input = take_file_input(parsed_args["fasta"])
        data = parse_fasta(input)
        profile =  calculate_profile_matrix(data)
        consensus = get_consensus(profile)
        
        # Formatting output
        println(consensus)
        print_profile(profile)
    catch
        throw("Input file could not be analysed.")
    end
end



# Functions -----------------------------------------------------------------

"""
    take_file_input(file :: String)

Take in a file name from the command line, and read it into a io file.
"""
function take_file_input(file :: String)
    if isfile(file)
        input = open(file, "r")
        return input
    else
        throw("provided $file is not a valid file.") 
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
    Profile_matrix

A data type to represent a profile matrix.

# Fields

- `matrix` : The profile matrix.
- `bases` : The letters associated with each index of the profile matrix.
"""
struct Profile_matrix
    matrix :: Matrix{Float64}
    bases :: Vector{Char}
end

"""
    profile_matrix(sequences::Vector{Dna})::Matrix

Construct a profile matrix from a vector of Dna sequences. This function makes
a profile by first iterating over the characters of all sequences and then
turning to a new sequence.

This function can be generalised to protein sequences.

...
# Arguments
- `sequences::Vector{Dna}::Matrix`: 

# Depends

- `check_equal_sequence_length()` : To check whether all sequences are of
similar length.
- `enumerate_characters()` : To determine the set of unique letters.

# Example
```julia
julia> fastas = fastas = parse_fasta(fasta_file_object)
julia> profile_matrix(fastas)
Profile_matrix([5.0 1.0 0.0 0.0 5.0 5.0 0.0 0.0; 1.0 1.0 6.0 3.0 0.0 1.0 0.0 0.0; 1.0 5.0 0.0 0 .0 0.0 1.0 1.0 6.0; 0.0 0.0 1.0 4.0 2.0 0.0 6.0 1.0], ['A', 'G', 'T', 'C']) 
```
"""
function calculate_profile_matrix(Dna_sequences :: Vector{Dna})::Profile_matrix
    check_equal_sequence_length(Dna_sequences) 
    sequences = map(x -> x.sequence, Dna_sequences)
    sequenceₗ = length(sequences[1])
    
    # Letters are the row names.
    letters = collect(keys(enumerate_characters(join(sequences))))
    # Pre-allocate all the values of the array.
    profile = zeros(length(letters), sequenceₗ)
    
    for index = 1:sequenceₗ
        for sequence_index = 1:length(sequences)
            for index_character = 1:length(letters)
                current_char = sequences[sequence_index][index]
                if string(current_char) == string(letters[index_character])
                    profile[index_character, index] += 1
                end
            end
        end

    end
    profile_matrix = Profile_matrix(profile, letters)
    return profile_matrix
end

"""
    max_rows_indeces(matrix::Matrix)::Array{Int}

Determine the index of the max value in each row in a column wise order.

...
# Arguments
- `matrix::Matrix::Array{Int}`: Matrix to be analysed

"""
function max_rows_indeces(matrix :: Matrix) :: Array{Int}
    max_row_list = []
    rows, cols = size(matrix)
    # Current max
    for c =  1:cols
        max_col = maximum(matrix[1:end, c])
        for r = 1:rows
            value = matrix[r, c] 
            if value == max_col
                # The current index is max
                push!(max_row_list, r)
            end
        end
    end
    return max_row_list
end

"""
    get_consensus(matrix,headers)::String

Convert a profile matrix into a consensus string.

...
# Arguments
- `profile_matrix` : A `Profile_matrix` object.
...

# Example
```julia
```
"""
function get_consensus(profile_matrix :: Profile_matrix) :: String
    matrix = profile_matrix.matrix
    bases = profile_matrix.bases

    indeces = max_rows_indeces(matrix)
    consensus = String(map(x -> bases[x], indeces))
    return consensus
end

function print_profile(profile_matrix :: Profile_matrix)
    rows, _ = size(profile_matrix.matrix)
    for r = 1:rows
        println("$(profile_matrix.bases[r]): "*
                "$(join(map(x -> floor(Int64, x),
                            profile_matrix.matrix[r, 1:end]), 
                              " "))")
    end
    
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






