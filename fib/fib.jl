#! /usr/bin/env julia
#= Solution for the Rabbits and Recurrence Relations problem

usage Comand line usage is as follows.
    ./fib.jl <n> <k>
description --- The rosalind problem https://rosalind.info/problems/fib/
=#


function recursive_rabbits_population(;
    n::Int,
    k::Int)
    #=Calculating the population of recursive rabbits.

    :n: int: The number of generations.
    :k: int: The breeding rate of each rabbit pair.
    :return: int: The number of rabbits after n generations with a breeding
        rate of k.
    =#

    population = [1, 0]
    global current_month = 1
    while current_month < n
        global population[2] = population[1] + population[2] * k
        current_population = population[2] 
        pushfirst!(population, current_population)
        # Going to the next month
        global current_month = current_month + 1
        # print(current_month, ' ', population, ' ', k, "\n")
    end
    return first(population)
end

function main()
    #= The main procedure
    :return: To standard out.
    =#
    if length(ARGS) != 2
        println("usage: $PROGRAM_FILE  <n> <k> \n"*
                "n \t The number of generations. \n"*
                "k \t The breeding rate of the recursive rabbits.")
        exit(1)
    end

    input_strings = ARGS
    n, k = map((x) -> parse(Int, x), input_strings)

    population = recursive_rabbits_population(n = n, k = k)
    println(population)
    exit(0)
end


if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
