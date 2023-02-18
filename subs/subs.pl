#! /usr/bin/env perl

# see
# https://stackoverflow.com/questions/87380/how-can-i-find-the-location-of-a-regex-match-in-perl
# for more information,

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

sub all_match_indeces {
    my ($pattern, $expression) = @_;
    my @indeces;
    while ($expression =~ /$pattern/g){
        push(@indeces, $-[0]);
        print $-[0], " ";
    }
}

sub main {
    #' The main subroutine.
    #'@input Taken by reading standard in; a text file/stream.
    #'@out printed to SDTOUT.
    my @input_strings;
    while (<>)
    {
        chomp;
        push @input_strings, $_; 
    }

    my ($subject, $query)  = @input_strings;
    my @indeces = all_match_indeces($query, $subject);
    my $output = "S: $subject \t Q: $query";
    print @indeces;
}

main()

