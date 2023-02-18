#! /usr/bin/env perl
#'@title Transcribing a DNA sequence.
#'@author Sibbe Bakker
#'@usage After making this script executable, you can use it as follows:
#' ./rna.pl <text file>
#'@description The solution to rosalind exercise 
#' https://rosalind.info/problems/rna/. Here a string of DNA is translated 
#' into RNA by replacing all occurrences of T into U. Next the RNA string is
#' translated into protein.
#'@linkout https://www.biob.in/2010/03/rna-to-protein-translation-in-perl_9.html?sc=1676744323929#c6108513225696985583

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

sub is_standard_rna {
    # Checking whether a string is a standard rna string
    #@param $string The string of text to be analysed. Can be lower case or
    #' upper case.
    #@return Bool True when $string is a valid DNA string. False if $string
    #' is not a valid DNA string.

    my ($string) = @_;
    my $is_rna = 0;
    # tr//x//c counts (tr) how many chrs are not in a set (c)
    if( 0 == uc($string) =~ tr/ACGU//c ) {$is_rna = 1; } 
    return $is_rna;
}

sub transcribe_string_to_rna {
    #'@title transcribing a string into RNA.
    #'@param $string A string of DNA. String can be lower case or upper case.
    #'@return $string The RNA string translated from the string given
    #' as input.

    my ($string) = @_;
    $string = uc $string;
    # Using the tr function to replace characters.
    $string =~ tr/T/U/;
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

sub translate_rna_to_protein {
    #'@title A subroutine to translate rna into protein

    my ($input) = @_;
    my $rna = is_standard_rna($input) ? $input : 
        die("Provided input is not RNA");
    my %codon_map = (
        # codon => amino acid
        'UUU'=> 'F', 'CUU'=> 'L', 'AUU'=> 'I', 'GUU'=> 'V',
        'UUC'=> 'F', 'CUC'=> 'L', 'AUC'=> 'I', 'GUC'=> 'V',
        'UUA'=> 'L', 'CUA'=> 'L', 'AUA'=> 'I', 'GUA'=> 'V',
        'UUG'=> 'L', 'CUG'=> 'L', 'AUG'=> 'M', 'GUG'=> 'V',
        'UCU'=> 'S', 'CCU'=> 'P', 'ACU'=> 'T', 'GCU'=> 'A',
        'UCC'=> 'S', 'CCC'=> 'P', 'ACC'=> 'T', 'GCC'=> 'A',
        'UCA'=> 'S', 'CCA'=> 'P', 'ACA'=> 'T', 'GCA'=> 'A',
        'UCG'=> 'S', 'CCG'=> 'P', 'ACG'=> 'T', 'GCG'=> 'A',
        'UAU'=> 'Y', 'CAU'=> 'H', 'AAU'=> 'N', 'GAU'=> 'D',
        'UAC'=> 'Y', 'CAC'=> 'H', 'AAC'=> 'N', 'GAC'=> 'D',
        'UAA'=> '*', 'CAA'=> 'Q', 'AAA'=> 'K', 'GAA'=> 'E',
        'UAG'=> '*', 'CAG'=> 'Q', 'AAG'=> 'K', 'GAG'=> 'E',
        'UGU'=> 'C', 'CGU'=> 'R', 'AGU'=> 'S', 'GGU'=> 'G',
        'UGC'=> 'C', 'CGC'=> 'R', 'AGC'=> 'S', 'GGC'=> 'G',
        'UGA'=> '*', 'CGA'=> 'R', 'AGA'=> 'R', 'GGA'=> 'G',
        'UGG'=> 'W', 'CGG'=> 'R', 'AGG'=> 'R', 'GGG'=> 'G');
    
    my $protein = "";

    for (my $codon_start=0, $codon_start-length($rna)-2; $i+=3)
    {
        $codon = substr $rna, $codon_start, 3;
        # Append to the $protein variable.
        $protein

    }

    print $codon_map{'UUG'};
    # performing the replacement: s(substitute)/(capture)/<replace>/g(lobal)
    # $protein =~ s/([$replacements])/$codon_map{$1}/g;
    return $protein

}

sub main { #' The main subroutine. 
    #'@input Taken by reading standard in; a text file/stream.
    #'@out printed to SDTOUT.
    my $input = <>;
    chomp $input;
    # my $rna = transcribe_dna_to_rna($input);
    my $rna = uc $input;
    my $protein = translate_rna_to_protein($rna);
    print STDOUT $protein;
}

main()

