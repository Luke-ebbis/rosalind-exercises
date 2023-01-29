#! /usr/bin/env julia
#= Solution for the Rabbits and Recurrence Relations problem

description --- The rosalind problem https://rosalind.info/problems/fib/
=#

input_lines = readlines()
input_string = split(input_lines[1], " ")
arguments = map((x) -> parse(Int, x),
                input_string)
n, k = arguments
print("n = $n, k = $k\n")


population = [1, 0]
current_month = 1
while current_month < n

    global population[2] = population[1] + population[2] * k
    current_population = population[2] 
    pushfirst!(population, current_population)

    # Going to the next month
    global current_month = current_month + 1

    # print(current_month, ' ', population, ' ', k, "\n")
    
end

