#!/usr/bin/perl
# produce sorting of 3-covers fromt the input files
# run like:
#
# ./setcovers3.pl `ls 000cg*`

use strict;

my %r=();
my %h=();

while (my $f=shift)
{
    open(F,$f) or die; my @l=<F>; my %g=(); @g{@l}=0; close(F);
    $h{$f} = \%g;
}

my @ff = sort keys %h;


foreach my $i (0 .. $#ff)
{
    foreach my $j ($i+1 .. $#ff)
    {
        foreach my $k ($j+1 .. $#ff)
        {
            foreach my $l ($k+1 .. $#ff)
            {
            foreach my $m ($l+1 .. $#ff)
            {
            
            
                my %t = ();
                @t{ keys( %{$h{$ff[$i]}}), keys (%{$h{$ff[$j]}}), keys (%{$h{$ff[$k]}}), keys (%{$h{$ff[$l]}}), keys (%{$h{$ff[$m]}}) } = ();
                $r{ "$ff[$i]:$ff[$j]::$ff[$k]:::$ff[$l]::::$ff[$m]" } = scalar keys %t;
            }
            }
        }
    }
}

foreach my $v (sort {$r{$a} <=> $r{$b}} keys %r)
{
    print "$v\t$r{$v}\n";
}

