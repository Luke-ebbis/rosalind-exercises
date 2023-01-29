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
    for (arg,val) in parsed_args
        println("  $arg  =>  $val")
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    

    main()


end
