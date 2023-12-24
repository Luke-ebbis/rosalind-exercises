#! /usr/bin/env perl
#'@title Enumerating unique bases in a standard DNA string
#'@author Sibbe Bakker
#'@description The solution for rosalind exercises
#' https://rosalind.info/problems/dna/.
#'@usage After making this script executable, you can use it as follows:
#'$ echo "ATATAGGGATGACCGA" > dna.txt 
#'$ echo "ATATAGGGATGACCGAY" > not_dna.txt 
#'$ ./dna.pl dna.txt
#' 6 2 5 3
#'$ ./dna.pl not_dna.txt
#' The input string is not a standard DNA molecule. at ./dna.pl line 57,
#' <> line 1

use feature q(say);
use strict;
use warnings;

sub is_standard_dna {
    # Checking whether a string is a standard DNA string
    #@param $string The string of text to be analysed.
    #@return Bool True when $string is a valid DNA string. False if $string
    #' is not a valid DNA string.

    my ($string) = @_;
    my $is_dna = 0;
    # tr//x//c counts (tr) how many chrs are not in a set (c)
    if( 0 == uc($string) =~ tr/ACGT//c ) {
        $is_dna = 1;
    } 
    return $is_dna;
}

sub count_letters {
    # Counting the number of unique letters in a string
    #'@param $string A string of text to be analysed.
    #'@return hash The number of letters per unique letter of the input 
    #' string

    my ($string) = @_;
    my @letters = split(//, $string);

    # Now the letters are put into a hash with the sums per key
    my %letter_count;

    ++$letter_count{$_} for @letters;
    return %letter_count
}

sub dna_count {
    # Obtaining the count of nucleotides obtained from a standard DNA string
    #'@param $string The string to be analysed.
    #'@return @array The counts for each base in the DNA sequence in the order
    #' A, C, G and T.
    #
    #'@description This function will only work for strings containing the
    #' standard bases of DNA: ACGT.
    #'@dependencies is_standard_dna(), count_letters();
    
    my ($input) = @_;

    # Check if the string is DNA or not.
    my $dna = is_standard_dna($input) ? $input : 
        die("The input string is not a standard DNA molecule.");
    
   my %dna_counts = count_letters($dna);
   my @base_counts = ($dna_counts{"A"}, 
                      $dna_counts{"C"},
                      $dna_counts{"G"},
                      $dna_counts{"T"});
    return @base_counts;
}

# Message for explaining shell usage.
my $use_msg = "usage: $0 <input_file> 
Obtaining the number of bases in a DNA sequence.

<input_file> \t A file holding a DNA sequence. If the given text file does 
\t not contain a standard DNA sequence, an error message is given.

 standard out: The amount of A, C, G, T bases in the input_file.";
# die "$use_msg" unless <> else my $input = <>;
unless(my $input  = <>) {
    print $use_msg;
} else {
    chomp $input;
    print STDOUT join(" ", dna_count($input)), "\n";
}

# my $input = <>;
