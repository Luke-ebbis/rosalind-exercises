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

#= Open the file into a string
=#
function open_dna_line(path::String)::String
    bytes = open(path, "r") do f
        bytes = read(f)
        return bytes
    end
    outst = String(bytes)
end

function dna_frequency(str::String)
    freq::Dict{Char, Number} = Dict(
                                    'A' => 0,
                                    'G' => 0,
                                    'C' => 0,
                                    'T' => 0,
                                   )
    # For each char in ...
    for char in collect.(str)
        freq[char] += 1
    end
    freq
end

function format_output(freq::Dict{Char, Number})::String
    "$(freq['A']) $(freq['C']) $(freq['G']) $(freq['T'])"
end

function main()
    filename::String = ARGS[1]
    input = open_dna_line(filename)
    cleaned = replace(input, "\n" => "")
    dna_frequency(cleaned) |> format_output |> print
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
