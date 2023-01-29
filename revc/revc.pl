#! /usr/bin/env perl
#'@title script to determine the reverse complement of a DNA string.
#'@author Sibbe Bakker
#'@usage ./revc.pl <text_file>
#'@description The solution to rosalind exercise 
#' https://rosalind.info/problems/revc/. Here the reverse complement of a DNA 
#' string is calculated. This means that it is reversed and complemented 

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

sub reverse_complement_dna {
    #'@title Getting the reverse complement of DNA
    #'@param $string A string of DNA. String can be lower case or upper case.
    #'@return $string The reverse complement of the input string.
    #
    #'@description This function will only work for strings containing the
    #' standard bases of DNA: ACGT.
    #'@dependencies is_standard_dna().
    
    my ($input) = @_;
    my $dna = is_standard_dna($input) ? $input : 
        die("The input string is not a standard DNA molecule.");
    my $dna_reverse = reverse($dna);
    my %complement_map = (
        A => 'T',
        T => 'A',
        C => 'G',
        G => 'C',
    );
    my $replacements = join('', keys(%complement_map));
    $dna_reverse =~ s/([$replacements])/$complement_map{$1}/g;
    return $dna_reverse
}

sub main {
    #' The main subroutine. 
    #'@input Taken by reading standard in; a text file/stream.
    #'@out printed to SDTOUT.
    my $input = <>;
    chomp $input;
    my $dna_reverse_complement = reverse_complement_dna($input);
    print STDOUT $dna_reverse_complement;
}

main()
