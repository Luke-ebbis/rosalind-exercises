#! /usr/bin/env julia
#=Determining the CG content of a DNA string
=#



"""
    main()

This is the main procedure.
"""
function main()

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

    data = parse_fasta(input)
    gc_values = map(x -> (x.identifier, calculate_gc(x)), data)
    sort!(gc_values,
          by=x->x[2])
    max_gc = last(gc_values)
    println(max_gc[1], "\n",  max_gc[2])
    exit(0)
end


# Functions -----------------------------------------------------------------

"""
    parse_fasta(inputObject::IOStream)

Parse a fasta file, return a list of Dna data types.

...
# Arguments
- `inputObject::IOStream`: An open fasta file object. As created with the
`open()` function.
...
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

"""
    calculate_gc(dna::Dna)

Calculate the GC percentage of a Dna object.

# Arguments
- `dna::Dna`: The Dna object to be investigated.
"""
function calculate_gc(dna :: Dna)
    counts = enumerate_characters(dna.sequence)
    sum_counts = counts['A'] + counts['T'] + counts['G'] + counts['C']
    gc_counts = counts['G'] + counts['C']
    cg_percentage = (gc_counts / sum_counts) * 100
    return cg_percentage
end


if abspath(PROGRAM_FILE) == @__FILE__
    # only executing the main function as a script
    main()
end
