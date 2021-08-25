#!/usr/bin/perl
# produce sorting of strategies according to agreedy coverage of problems
# run like:
#
# ./greedycover.pl `ls 000cg*`

use strict;

my %r=();
my %h=();

while (my $f=shift)
{
    open(F,$f) or die; my @l=<F>; my %g=(); @g{@l}=0; close(F);
    $h{$f} = \%g;
}

my $max = scalar keys %h;
my @sorted = keys %h;


foreach my $j (1 .. $max)
{
    @sorted = sort {scalar (keys %{$h{$b}}) <=>  scalar (keys %{$h{$a}})} @sorted;

    my $top = shift @sorted;

    print "$top :",  scalar (keys %{$h{$top}}), "\n";

    my @todelete = keys %{$h{$top}};

    foreach my $i (@sorted)
    {
    
	delete @{$h{$i}}{  @todelete };

    }
}




