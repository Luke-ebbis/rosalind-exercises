#! /usr/bin/env julia
"""Finding the motives in a string.
author --- Sibbe Bakker
"""

"""The main function
"""
function main()
    # The main function.
    if length(ARGS) != 1
        println("usage: $PROGRAM_FILE  <input> \n"*
                "input \t File path to a input file, containing.\n"*
                "      \t Two lines: first line is the query, second line \n"*
                "      \t is the query."
               )
        exit(1)
    end
    input = readlines(ARGS[1])
    if length(input) == 2
        subject, query = input
        match_indices = find_exact_match(subject, query)
        output = join(match_indices, " ")
        println(output)
    else
        println("provided file ($(ARGS[1]) should only have two lines.")
    end
end


"""
    kmers(subject::String,kmer_size::Int64)::Tuple

Calculates _k_-mers for the `subject` string as a tuple of form `Tuple(String,
(Int, Int)`, where the two integers refer to start and end index (1 indexed and
inclusive). A string of length _L_ will have _Nₖ_ = _L_ - _k_ + 1 _k_-mers.
...
# Arguments
- `subject::String`: The string that should be broken into _k_-mers.
- `kmer_size::Int64`: Variable to set the _k_-mer size.
# Throws
- `ArgumentError`: When the length of the `subject` ≤ `kmer_size`.

...

# Example
```julia-repl
julia> kmers("AAAAGGGAGGGGCTGCAG", 3)
16-element Vector{Tuple{String, Tuple{Int64, Int64}}}:
 ("AAA", (1, 3))
 ("AAA", (2, 4))
 ("AAG", (3, 5))
 ("AGG", (4, 6))
 ("GGG", (5, 7))
 ("GGA", (6, 8))
 ("GAG", (7, 9))
 ("AGG", (8, 10))
 ("GGG", (9, 11))
 ("GGG", (10, 12))
 ("GGC", (11, 13))
 ("GCT", (12, 14))
 ("CTG", (13, 15))
 ("TGC", (14, 16))
 ("GCA", (15, 17))
 ("CAG", (16, 18))
```
"""
function kmers(subject :: String, 
        kmer_size :: Int64) :: Vector{Tuple{String, Tuple{Int64, Int64}}}
    #TODO specify the kmer list in the same type as the return 
    kmers_list = []
    index_start, index_end = 0, 0
    
    if length(subject) <=  kmer_size 
       throw(ArgumentError(
                  "Value of k ($kmer_size) may not be bigger or equal " *
                  "to the length of the subject sequence " *
                  "($(length(subject)))."
                 )
            )
    end
    i = 1;

    # Make kmers until we go over the length of the subject.
    while i + kmer_size != length(subject) + 2 
        kmerᵢ = subject[i : i + kmer_size - 1]
        index_start = i
        index_end = i + kmer_size - 1
        # println("$i \t $(subject[i]) \t $kmerᵢ"*
        #        "\t $index_start -> $index_end")
        push!(kmers_list,
              (kmerᵢ, (index_start, index_end)))
        i += 1
    end
    return kmers_list
end

"""
    find_exact_match(subject::String,query::String)::Vector{Int64}

Fetching all starting indices of the query within a subject string (1 indexed)
using _k_-mer string matching.

# Arguments
- `subject::String`: String to be searched for `query` matches.
- `query::String`: The query.

# Throws
- `ArgumentError` : If length of `query` ≥ `subject`.
# Depends
- `kmers()`

# Example
```julia-repl
julia> find_exact_match("GATATATGCATATACTT", "ATAT")
3-element Vector{Int64}
  2
  4
 10
```
"""
function find_exact_match(subject :: String, query :: String) :: Vector{Int64}
    starting_locations = []
    queryₗ = length(query)
    if length(subject) <= queryₗ 
       throw(ArgumentError(
                   "Length of query $(queryₗ) may not be ≥ " *
                   "length of subject $(length(subject))."
                 )
            )
    end

    # Making kmers
    kmers_list = kmers(subject, queryₗ)
    for kmer in kmers_list
        if kmer[1] == query
            push!(starting_locations,
                  kmer[2][1])
        end
    end
    return starting_locations
end


if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
