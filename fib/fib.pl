#! /usr/bin/env perl
#'@title script to determine the reverse complement of a DNA string.
#'@author Sibbe Bakker
#'@usage ./fib.pl <n> <k>
#'@description The solution to rosalind exercise
#' https://rosalind.info/problems/fib/. Here the reverse complement of a DNA
#' string is calculated. This means that it is reversed and complemented

use strict;
use warnings;
use feature q(say);

sub calculate_population_recursive_rabbits{
    #' Calculating the population of recursive rabbits.
    #'@param $n The number of generations.
    #'@param $k The breeding rate per fertile pair.
    #'@return The number of rabbits after n generations with a breeding rate 
    #' of k.
    
    my ($n, $k) = @_;
    # say "Using a breeding rate of $k over $n generations.";
    
    my @population = (1, 0);
    my $n_current = 1;
    while ($n_current < $n) {
        $population[1] = $population[0] + $population[1] * $k;
        my $current_population = $population[1];
        unshift  @population, $current_population; 
        # Going to the next generation.
        $n_current++;
        # say "$n_current \t ", join(" ", @population);
    }

    return shift @population;
}

sub main {
    #' The main procedure.
    
    # Argument handling.
    die "usage: $0 <n> <k>\n" unless @ARGV == 2;
    my ($n, $k) = @ARGV;

    #Calculating the number of rabbits.
    my $population = calculate_population_recursive_rabbits($n, $k);
    say STDOUT $population;
}

unless (caller) {
    # Execute the main function is this is the code being run
    main()
}
