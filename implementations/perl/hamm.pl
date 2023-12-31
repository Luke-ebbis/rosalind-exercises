#! /usr/bin/env perl
use feature q(say);
use strict;
use warnings;

# The hamming distance
sub hamm {
    # q and s are both strings.
    my ($q, $s) = @_;
    chomp $q; chomp $s;

    # Check if the requirement holds that the lengths are equal...
    my $length_eq  = length($q) == length($s);
    die "length between input strings must be equal\n" unless($length_eq);
    
    my $differences = 0;
    my @q_list = split //, $q;
    my @s_list = split //, $s;

    for my $i (0 .. $#q_list) {
      my $first  = $q_list[$i];
      my $second = $s_list[$i];
      unless($first eq $second){
          $differences += 1;
      }
    }
    return $differences;
}

sub main {
    my @dnas = <>;
    my $query = $dnas[0];
    my $subject = $dnas[1];
    my $distance = hamm $query, $subject;
    print "$distance\n";
}

main()
