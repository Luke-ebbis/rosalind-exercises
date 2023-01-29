#! /usr/bin/env perl
#'@title Transcribing a DNA sequence.
#'@author Sibbe Bakker
#'@description The solution to rosalind exercise 
#' https://rosalind.info/problems/rna/. Here a string of DNA is translated 
#' into RNA by replacing all occurrences of T into U.

use strict;
use warnings;

sub is_standard_dna {
    # Checking whether a string is a standard DNA string
    #@param $string The string of text to be analysed. Can be lower case or
    #' upper case.
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

sub transcribe_string_to_rna {
    #'@title transcribing a string into RNA.
    #'@param $string A string of DNA. String can be lower case or upper case.
    #'@return $string The RNA string translated from the string given
    #' as input.

    my ($string) = @_;
    $string = uc $string;
    # Using the tr function to replace characters.
    $string =~ tr/A/U/;
    return $string;
}

sub transcribe_dna_to_rna {
    #'@title transcribing a string into RNA.
    #'@param $string A string of DNA. String can be lower case or upper case.
    #'@return $string The RNA string translated from the DNA string given
    #' as input.
    #'@description This function will only work for strings containing the
    #' standard bases of DNA: ACGT.
    #'@dependencies is_standard_dna(), transcribe_string_to_rna().
    
    my ($input) = @_;
    my $dna = is_standard_dna($input) ? $input : 
        die("The input string is not a standard DNA molecule.");
    my $rna = transcribe_string_to_rna($dna);
    return $rna
}

sub main {
    #' The main subroutine. 
    #'@input Taken by reading standard in; a text file/stream.
    #'@out printed to SDTOUT.
    my $input = <>;
    chomp $input;
    my $rna = transcribe_dna_to_rna($input);
    print STDOUT $rna;
}

main()

