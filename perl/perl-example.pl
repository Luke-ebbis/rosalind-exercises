#! /usr/bin/env perl
sub match_positions {
    my ($regex, $string) = @_;
    return if not $string =~ /$regex/;
    return ($-[0], $+[0]);
}
sub match_all_positions {
    my ($regex, $string) = @_;
    my @ret;
    while ($string =~ /$regex/g) {
        push @ret, [ $-[0], $+[0] ];
    }
    return @ret
}


